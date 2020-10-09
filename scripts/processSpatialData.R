### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Cleaning spatial data obtained from client

##  Load packages
library(raster) # Raster packages deals with tiff files (grid files)
library(tidyverse)

# -------------------------------------------------------------------------

## Load spatial data
# Mapzones for north west GeoRegion
nwMapzones <- raster("data/raw/nw_mapzone_tiff/nw_mapzone.tif")

# EVT, EVH, EVC
nwEVT <- raster("data/raw/nw_evt2.0_tiff/nw_evt20.tif")
nwEVH <- raster("data/raw/nw_evh2.0class1.4_tiff/nw_evh_m.tif")
nwEVC <- raster("data/raw/nw_evc2.0class1.4_tiff/nw_evc_m.tif")

# fDIST
fdist <- raster("data/raw/NW_FDIST2014_TIFF/nw_fdist2014.tif")

# Change the origin of mapzone raster
origin(nwMapzones) <- origin(nwEVT)

# -------------------------------------------------------------------------

## Crop data to Mapzones
nwEVTCropped <- crop(nwEVT, nwMapzones)
nwEVHCropped <- crop(nwEVH, nwMapzones)
nwEVCCropped <- crop(nwEVC, nwMapzones)
fdistCropped <- crop(fdist, nwMapzones)

## Save clean data
writeRaster(nwEVTCropped, "data/clean/nw_EVT_clean.tif",
            overwrite = TRUE)
writeRaster(nwEVHCropped, "data/clean/nw_EVH_clean.tif",
            overwrite = TRUE)
writeRaster(nwEVCCropped, "data/clean/nw_EVC_clean.tif",
            overwrite = TRUE)
writeRaster(fdistCropped, "data/clean/nw_fDIST_clean.tif",
            overwrite = TRUE)

# -------------------------------------------------------------------------

# Load Clean data
nwEVTCropped <- raster("data/clean/nw_EVT_clean.tif")
nwEVHCropped <- raster("data/clean/nw_EVH_clean.tif")
nwEVCCropped <- raster("data/clean/nw_EVC_clean.tif")
fdistCropped <- raster("data/clean/nw_fDIST_clean.tif")

## Crop data to smaller extent
# Create smaller extent
theExt <- extent(-1417400, -1371131, 2614395, 2655111)

# Crop
nwMapzonesSmall <- crop(nwMapzones, theExt/2)
nwEVTMaskedSmall <- crop(nwEVTCropped, theExt/2)
nwEVHMaskedSmall <- crop(nwEVHCropped, theExt/2)
nwEVCMaskedSmall <- crop(nwEVCCropped, theExt/2)
fdistMaskedSmall <- crop(fdistCropped, theExt/2)

## Save cropped data
writeRaster(nwMapzonesSmall, "data/clean/cropped/nw_Mapzones_small.tif",
            overwrite = TRUE)
writeRaster(nwEVTMaskedSmall, "data/clean/cropped/nw_EVT_clean_small.tif",
            overwrite = TRUE)
writeRaster(nwEVHMaskedSmall, "data/clean/cropped/nw_EVH_clean_small.tif",
            overwrite = TRUE)
writeRaster(nwEVCMaskedSmall, "data/clean/cropped/nw_EVC_clean_small.tif",
            overwrite = TRUE)
writeRaster(fdistMaskedSmall, "data/clean/cropped/nw_fDIST_clean_small.tif",
            overwrite = TRUE)

# -------------------------------------------------------------------------

## Layerizing FDIST

fdist <- raster("data/clean/cropped/nw_fDIST_clean_small.tif")

fdistStack <- layerize(fdist)
layerValues <- as.numeric(str_remove(names(fdistStack), "X"))
multiplierFileNames <- paste0("data/clean/cropped/FDIST/FDIST_value_", 
                              layerValues, ".tif")

mapply(writeRaster, 
       filename = multiplierFileNames, 
       x = as.list(fdistStack), 
       overwrite=T)

# -------------------------------------------------------------------------

## Create composite state class map

nwEVCMaskedSmall <- raster("data/clean/cropped/nw_EVC_clean_small.tif")
nwEVHMaskedSmall <- raster("data/clean/cropped/nw_EVH_clean_small.tif")

# Formula is ECV * 1000 + EVH

stateClasses <- nwEVCMaskedSmall*1000 + nwEVHMaskedSmall
writeRaster(stateClasses, "data/clean/cropped/nw_EVC_EVH_StateClasses.tif",
            overwrite = TRUE)
