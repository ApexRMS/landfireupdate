### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Cleaning spatial data obtained from client

##  Load packages
library(raster) # Raster packages deals with tiff files (grid files)

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

## Crop data to Mapzones
nwEVTCropped <- crop(nwEVT, nwMapzones)
nwEVHCropped <- crop(nwEVH, nwMapzones)
nwEVCCropped <- crop(nwEVC, nwMapzones)
fdistCropped <- crop(fdist, nwMapzones)

## Mask after cropping
nwEVTMasked <- mask(nwEVTCropped, nwMapzones)
nwEVHMasked <- mask(nwEVHCropped, nwMapzones)
nwEVCMasked <- mask(nwEVCCropped, nwMapzones)
fdistMasked <- mask(fdistCropped, nwMapzones)

## Save clean data
writeRaster(nwEVTMasked, "data/clean/nw_EVT_clean.tif",
            overwrite = TRUE)
writeRaster(nwEVHMasked, "data/clean/nw_EVH_clean.tif",
            overwrite = TRUE)
writeRaster(nwEVCMasked, "data/clean/nw_EVC_clean.tif",
            overwrite = TRUE)
writeRaster(fdistMasked, "data/clean/nw_fDIST_clean.tif",
            overwrite = TRUE)

## Crop data to smaller extent
# Create smaller extent
theExt <- extent(-1417400, -1371131, 2614395, 2655111)

# Crop
nwMapzonesSmall <- crop(nwMapzones, theExt)
nwEVTMaskedSmall <- crop(nwEVTMasked, theExt)
nwEVHMaskedSmall <- crop(nwEVHMasked, theExt)
nwEVCMaskedSmall <- crop(nwEVCMasked, theExt)
fdistMaskedSmall <- crop(fdistMasked, theExt)

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
