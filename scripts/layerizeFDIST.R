### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### October 2020
### Layerizing FDIST

##  Load packages
library(raster)
library(tidyverse)

fdist <- raster("data/clean/cropped/nw_fDIST_clean_small.tif")

fdistStack <- layerize(fdist)
layerValues <- as.numeric(str_remove(names(fdistStack), "X"))
multiplierNames <- paste0("data/clean/cropped/FDIST/FDIST_value_", 
                          layerValues, ".tif")

mapply(writeRaster, 
       filename = multiplierNames, 
       x = as.list(fdistStack))
