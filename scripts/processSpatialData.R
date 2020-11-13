### LANDFIRE Project 
### APEX RMS - Valentin Lucet and Shreeram Senthivasan 
### September 2020
### This script is used to clean and pre-process raw spatial data obtained from 
### LANDFIRE for simulation in SyncroSim

# Load packages ------------------------------------------------------------
library(raster) # Raster packages deals with tiff files (grid files)
library(tidyverse)
library(furrr)  # For parallel iteration
library(readxl) # For reading disturbance crosswalk

# Load global options ----------------------------------------------------

# Global options are set in the header file
source("scripts/constants.R")

# Load custom raster functions -------------------------------------------

# A number of custom raster functions are defined in the rasterFunctions script
# that are optimized for large rasters
source("scripts/rasterFunctions.R")

# Load non-spatial data --------------------------------------------------
distCrosswalk <-   read_xlsx(distCrosswalkPath) %>%
  mutate(name = paste(d_type...2, d_severity...3, d_time...4, sep = " - ")) %>% 
  select(fdist = FDIST, vdist = VDIST, name)
# Load spatial data -------------------------------------------------------

# Mapzones for north west GeoRegion
mapzoneRaster <- raster(mapzoneRawRasterPath)

# EVT, EVH, EVC
evtRaster <- raster(evtRawRasterPath)
evhRaster <- raster(evhRawRasterPath)
evcRaster <- raster(evcRawRasterPath)

# fdistRaster
fdistRaster <- raster(fdistRawRasterPath)

# Change the origin of mapzone raster
origin(mapzoneRaster) <- origin(evtRaster)


# Setup mask --------------------------------------------------------------

# Mask and trim mapzone map by the chosen mapzone
# - Note that trimRaster also saves the raster to the cleaned raster directory
mapzoneRaster <-
  maskByMapzone(
    inputRaster = mapzoneRaster, 
    maskValue = mapzoneToKeep,
    filename = "temp.tif") %>%
  trimRaster(
    filename = mapzoneRasterPath
  )

# Remove temp files made during masking
unlink("temp.tif")

# Crop and mask data ----------------------------------------------------------

# Begin parallel processing
plan(multisession, workers = nThreads)

# Use a named list to define the rasters and file names to iterate over as we
# crop, mask, and write the outputs to file
rasterList <- list(
  "EVT" = evtRaster,
  "EVH" = evhRaster,
  "EVC" = evcRaster,
  "fDIST" = fdistRaster) %>%
  future_imap(
    function(raster, name, maskRaster){
      crop(raster, maskRaster) %>%
        mask(maskRaster) %>%
        writeRaster(
          filename = paste0(cleanRasterDirectory, name, ".tif"),
          overwrite = TRUE
        )
    },
    maskRaster = mapzoneRaster,
    .options = furrr_options(
      seed = TRUE,
      packages = "raster"
    )
  )

# End parallel processing
plan(sequential)

# Assign the results from the list back to the original variables
evtRaster <- rasterList$EVT
evhRaster <- rasterList$EVH
evcRaster <- rasterList$EVC
fdistRaster <- rasterList$fDIST
rm(rasterList)

# Convert disturbance to VDIST ------------------------------------------------

# Choose number of blocks to split rasters into when processing to limit memory
fdistBlockInfo <- blockSize(fdistRaster)

# Begin parallel processing
plan(multisession, workers = nThreads)

# Split the FDIST raster into blocks to calculate the unique set of FDIST codes
# present in the data without using excessive memory per thread
fdistLevels <- 
  future_map(
    seq(fdistBlockInfo$n),
    ~ unique(getValuesBlock(fdistRaster,
                            row   = fdistBlockInfo$row[.x],
                            nrows = fdistBlockInfo$nrows[.x])),
    .options = furrr_options(
      seed = TRUE,
      globals = c("fdistRaster", "fdistBlockInfo"),
      packages = "raster"
      )) %>%
  flatten_dbl %>%
  unique() %>%
  `[`(!. %in% c(0, NA)) # Remove NA (no data) and 0 (no disturbance)

# End parallel processing
plan(sequential)

# Check which fdist codes need to be reclassified to a different vdist code
distReclassification <- distCrosswalk %>%
  filter(
    fdist %in% fdistLevels,
    fdist != vdist) %>%
  select(-name) %>%
  as.matrix

# Reclassify as necessary and save
vdistRaster <- reclassify(fdistRaster, distReclassification)
writeRaster(vdistRaster,
            vdistRasterPath,
            overwrite = TRUE)

# Generate list of unique VDIST codes
vdistLevels <- distCrosswalk %>%
  filter(fdist %in% fdistLevels) %>%
  pull(vdist) %>%
  unique %>%
  sort 

vdistNames <- distCrosswalk %>%
  select(-fdist) %>%
  filter(vdist %in% vdistLevels) %>%
  arrange(vdist) %>%
  unique() %>%
  pull(name)

# Layerize disturbace raster -------------------------------------------------


# Begin parallel processing
plan(multisession, workers = nThreads)

# Split the VDIST raster into binary layers
# One layer is constructed at a time per thread to limit memory use per thread
future_walk2(
  vdistLevels,
  vdistNames,
  saveDistLayer,
  fullRaster = vdistRaster,
  .options = furrr_options(seed = TRUE))

# Return to sequential operation
plan(sequential)

# Create composite state class map ----------------------------------------

# EVC and EVH codes are both three digits
# We can "paste" these codes together by multiplying EVC by 1000 and summing
stateClasses <- evcRaster * 1000 + evhRaster
writeRaster(stateClasses,
            stateClassRasterPath,
            overwrite = TRUE)

# Tiling for spatial multiprocessing -------------------------------------

# Generate and write the tiling raster to file
tileRaster <- 
  tilize(
    mapzoneRaster, 
    tilingRasterPath,
    nx = tileCols)
