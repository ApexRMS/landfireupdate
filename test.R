library(rgrass7)
library(raster)

initGRASS(gisBase = "C:/Program Files/GRASS GIS 7.8", gisDbase = "test",  
          location = "test", mapset = "PERMANENT", override = TRUE)
nwEVTCropped <- raster("../data/clean/nw_EVT_clean.tif")
execGRASS("g.mapset", mapset="testtiling", flags="c")

execGRASS("r.in.gdal", input = filename(nwEVTCropped), output = "EVT", 
          flag = "o")

execGRASS("g.region", raster = "EVT")
execGRASS("r.tile", input = "EVT", output="EVT_tiled_", 
          width = 3866, height=5766)
