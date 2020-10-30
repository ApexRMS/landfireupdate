### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Cleaning spatial data obtained from client

## This code requires GRASS. Download GRASS here:
## https://grass.osgeo.org/download/
## On windows/Mac, reboot your computer after the installation

##  Load packages
library(raster) # Raster packages deals with tiff files (grid files)
library(tidyverse)
library(rgrass7)

# Load spatial data -------------------------------------------------------

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

# Crop data to Mapzones ---------------------------------------------------

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

# Load Clean data ---------------------------------------------------------

nwMapzones <- raster("data/raw/nw_mapzone_tiff/nw_mapzone.tif")
nwEVTCropped <- raster("data/clean/nw_EVT_clean.tif")
nwEVHCropped <- raster("data/clean/nw_EVH_clean.tif")
nwEVCCropped <- raster("data/clean/nw_EVC_clean.tif")
fdistCropped <- raster("data/clean/nw_fDIST_clean.tif")

# Smaller extent ----------------------------------------------------------

## Crop data to smaller extent
# Create smaller extent
theExt <- extent(-1397040,-1394113, 2634556, 2637015)
factor <- 1

# Crop
nwMapzonesSmall <- crop(nwMapzones, theExt/factor)
nwEVTMaskedSmall <- crop(nwEVTCropped, nwMapzonesSmall)
nwEVHMaskedSmall <- crop(nwEVHCropped, nwMapzonesSmall)
nwEVCMaskedSmall <- crop(nwEVCCropped, nwMapzonesSmall)
fdistMaskedSmall <- crop(fdistCropped, nwMapzonesSmall)

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

## Layerizing FDIST

fdist <- raster("data/clean/cropped/nw_fDIST_clean_small.tif")

# For small extent, it is okay to use raster::layerize

fdistStack <- layerize(fdist)
layerValues <- as.numeric(str_remove(names(fdistStack), "X"))
multiplierFileNames <- paste0("data/clean/cropped/FDIST/FDIST_value_", 
                              layerValues, ".tif")

# Use mapply to save all files with coded name
mapply(writeRaster, 
       filename = multiplierFileNames, 
       x = as.list(fdistStack), 
       overwrite=T)

## Create composite state class map

nwEVCMaskedSmall <- raster("data/clean/cropped/nw_EVC_clean_small.tif")
nwEVHMaskedSmall <- raster("data/clean/cropped/nw_EVH_clean_small.tif")

# Formula is ECV * 1000 + EVH

stateClasses <- nwEVCMaskedSmall*1000 + nwEVHMaskedSmall
writeRaster(stateClasses, "data/clean/cropped/nw_EVC_EVH_StateClasses.tif",
            overwrite = TRUE)

# Tiling for spatial multiprocessing

nCell <- ncell(fdist)
nCores <- 10

tiles <- fdist
values(tiles) <- rep(1:nCores, each = nCell/nCores)[1:nCell]

writeRaster(tiles, "data/clean/cropped/Tiling_small.tif", overwrite = TRUE)

# MAPZONE 19 Extent -------------------------------------------------------

# GRASS initiating the session
initGRASS(gisBase = "C:/Program Files/GRASS GIS 7.8", gisDbase = "grass",  
          location = "LF", mapset = "PERMANENT", override = TRUE)

# Setting the right projection and creating new mapset
execGRASS("g.proj", proj4 = projection(nwEVTCropped), flags="c")
execGRASS("g.mapset", mapset="Mapzone19", flags="c")

# Import all data
execGRASS("r.in.gdal", input = filename(nwMapzones), output = "MZ", 
          flag = "o")
execGRASS("r.in.gdal", input = filename(fdistCropped), output = "fDIST", 
          flag = "o")
execGRASS("r.in.gdal", input = filename(nwEVTCropped), output = "EVT", 
          flag = "o")
execGRASS("r.in.gdal", input = filename(nwEVCCropped), output = "EVC", 
          flag = "o")
execGRASS("r.in.gdal", input = filename(nwEVHCropped), output = "EVH", 
          flag = "o")

# execGRASS("g.extension", extension = "r.clip", operation = "add")

# Set the computational region
execGRASS("g.region", raster = "MZ")

# Create binary raster and reclassify
execGRASS("r.mapcalc", expression = "MZ_19=(MZ==19)", 
          flags = c("overwrite"))
execGRASS("r.null", map = "MZ_19", setnull = "0")

write_lines(c("1 = 19", "* = *"), "grass/rule.txt")
execGRASS("r.reclass", 
          input = "MZ_19", 
          rules = "grass/rule.txt",
          output = "MZ_19_rcl", 
          flags = c("overwrite"))

# Copy reclassified map (necessary to operate on it in GRASS)
execGRASS("r.mapcalc", expression = "MZ_19_rcl_new = MZ_19_rcl")

# Vectorize the result to change the region of computation just after
execGRASS("r.to.vect", input = "MZ_19_rcl_new", output = "MZ_19_vect", type = "area", 
          flags = c("overwrite"))
execGRASS("g.region", vector = "MZ_19_vect")

# Export only in the region (way to crop data)
execGRASS("r.out.gdal", input = "MZ_19_rcl_new", 
          output = "data/clean/cropped/nw_Mapzones_MZ19.tif", 
          flags = c("overwrite"))
execGRASS("r.out.gdal", input = "EVT", 
          output = "data/clean/cropped/nw_EVT_clean_MZ19.tif", 
          flags = c("overwrite"))
execGRASS("r.out.gdal", input = "fDIST", 
          output = "data/clean/cropped/nw_fDIST_clean_MZ19.tif", 
          flags = c("overwrite"))

execGRASS("r.mapcalc", expression = "StateClass=((EVC*1000)+EVH)",
          flags = c("overwrite"))
execGRASS("r.out.gdal", input = "StateClass", 
          output = "data/clean/cropped/nw_EVC_EVH_StateClasses_MZ19.tif", 
          flags = c("overwrite"))

# Masking (re importing in R to do it due to a bug in rgrass7 which prevents us
# from doing it in R)
MZ19 <- raster("data/clean/cropped/nw_Mapzones_MZ19.tif")
EVT19 <- raster("data/clean/cropped/nw_EVT_clean_MZ19.tif")
Fdist19 <- raster("data/clean/cropped/nw_fDIST_clean_MZ19.tif")
StateClass19 <- raster("data/clean/cropped/nw_EVC_EVH_StateClasses_MZ19.tif")

EVT19Masked <- mask(EVT19, MZ19)
Fdist19Masked <- mask(Fdist19, MZ19)
StateClass19Masked <- mask(StateClass19, MZ19) 

writeRaster(EVT19Masked, "data/clean/cropped/nw_EVT_clean_MZ19.tif", 
            overwrite = TRUE)
writeRaster(Fdist19Masked, "data/clean/cropped/nw_fDIST_clean_MZ19.tif", 
            overwrite = TRUE)
writeRaster(StateClass19Masked, "data/clean/cropped/nw_EVC_EVH_StateClasses_MZ19.tif", 
            overwrite = TRUE)

# Layerizing in a loop
for (val in unique(Fdist19Masked)){
  if (val > 0) {
    theName <- paste0("FDIST_value_", val)
    file <- paste0("data/clean/cropped/FDIST/MZ19/", theName, ".tif")
    execGRASS("r.mapcalc", expression = paste0(theName, "=(fDIST==", val, ")"), 
              flags = c("overwrite"))
    execGRASS("r.out.gdal", input = theName,
              output = file, 
              flags = c("overwrite"))
  }
}

# Tiling for spatial multiprocessing --------------------------------------

nCell <- ncell(raster("data/clean/cropped/nw_fDIST_clean_MZ19.tif"))
nCores <- 10

tiles <- raster("data/clean/cropped/nw_fDIST_clean_MZ19.tif")
# TODO NA values
values(tiles) <- rep(1:nCores, each = nCell/nCores)[1:nCell]

tilesMasked <- mask(tiles, MZ19)
writeRaster(tiles, "data/clean/cropped/Tiling_MZ19.tif", overwrite = TRUE)

# Cropping MZ19 files for the sake of testing -----------------------------

MZ19 <- raster("data/clean/cropped/nw_Mapzones_MZ19.tif")
EVT19 <- raster("data/clean/cropped/nw_EVT_clean_MZ19.tif")
Fdist19 <- raster("data/clean/cropped/nw_fDIST_clean_MZ19.tif")
StateClass19 <- raster("data/clean/cropped/nw_EVC_EVH_StateClasses_MZ19.tif")

x <- 220000
theExt <- extent(-1378715, -1354773+x, 2909746, 2938249+x)

MZ19Cropped <- crop(MZ19, theExt)
EVT19Cropped <- crop(EVT19, theExt)
Fdist19Cropped <- crop(Fdist19, theExt)
StateClass19Cropped <- crop(StateClass19, theExt)

writeRaster(MZ19Cropped, "data/clean/cropped/nw_Mapzones_MZ19_cropped.tif", 
            overwrite = TRUE)
writeRaster(EVT19Cropped, "data/clean/cropped/nw_EVT_clean_MZ19_cropped.tif", 
            overwrite = TRUE)
writeRaster(Fdist19Cropped, "data/clean/cropped/nw_fDIST_clean_MZ19_cropped.tif", 
            overwrite = TRUE)
writeRaster(StateClass19Cropped, "data/clean/cropped/nw_EVC_EVH_StateClasses_MZ19_cropped.tif",
            overwrite = TRUE)

for (file in list.files("data/clean/cropped/FDIST/MZ19/", full.names = TRUE)){
  if (!stringr::str_detect(basename(file), "cropped")){
    rast <- raster(file)
    theName <- paste0("data/clean/cropped/FDIST/MZ19/", 
                      tools::file_path_sans_ext(basename(file)),"_cropped.tif")
    writeRaster(crop(rast, theExt), theName, overwrite = TRUE)
  }
}

# Redo the tiling for cropped files with less cores this time

nCell <- ncell(raster("data/clean/cropped/nw_fDIST_clean_MZ19_cropped.tif"))
nCores <- 3

tiles <- raster("data/clean/cropped/nw_fDIST_clean_MZ19_cropped.tif")
values(tiles) <- rep(1:nCores, each = nCell/nCores)[1:nCell]
writeRaster(tiles, "data/clean/cropped/Tiling_MZ19_cropped.tif", overwrite = TRUE)

# -------------------------------------------------------------------------

# tiles <- nwEVTCropped
# nCells <- which(!is.na(nwEVTCropped))
# nCores <- 60
# 
# if (nCells %% nCores-1 > 0){
#   tileSize <- trunc(nCells / (nCores-1))
#   lastTileSize <- nCells - (tileSize * (nCores-1))
#   tileSizes <- c(rep(tileSize, nCores-1), lastTileSize)
# }
# 
# for (tile in 1:nCores){
#   
#   from <- 1 + tileSize*tile
#   to <- tileSize*tile
#   
#   tiles[from:to] <- tile
#   print(tile)
# }
