### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### November 2020
### The function defined in this script is used to convert disturbance rasters
### into binary rasters for each disturbance level. These will later be used by
### SyncroSim as transition spatial multipliers

layerizeDisturbance <- function(runTag) {
  # Generate run-specific file paths ---------------------------------------

  # Directory to store cleaned rasters
  # Note that the working directory is prepended since SyncroSim needs absolute paths
  cleanRasterDirectory <- paste0(getwd(), "/data/clean/", runTag, "/")

  # Directory and prefix for FDIST binary rasters (spatial multipliers)
  transitionMultiplierDirectory <- paste0(cleanRasterDirectory, "transitionMultipliers/")
  dir.create(transitionMultiplierDirectory, showWarnings = F)

  # Clean Raster Paths
  vdistRasterPath <- paste0(cleanRasterDirectory, "VDIST.tif")

  # VDIST info for layerizing
  vdistInfoPath <- paste0(cleanRasterDirectory, "VDIST.csv")

  # Load Data -----------------------------------------------------------------
  vdistInfo <- read_csv(vdistInfoPath)
  vdistRaster <- raster(vdistRasterPath)

  # Layerize disturbace raster -------------------------------------------------

  # Begin parallel processing
  plan(multisession, workers = nThreads)

  # Split the VDIST raster into binary layers
  # One layer is constructed at a time per thread to limit memory use per thread
  future_walk2(
    vdistInfo$vdist,
    vdistInfo$name,
    saveDistLayer,
    fullRaster = vdistRaster,
    transitionMultiplierDirectory = transitionMultiplierDirectory,
    .options = furrr_options(seed = TRUE))

  # Return to sequential operation
  plan(sequential)
}
