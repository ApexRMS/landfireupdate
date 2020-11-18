### LANDFIRE Project
### APEX RMS - Valentin Lucet and Shreeram Senthivasan
### September 2020
### The function defined in this script is used to clean and pre-process raw
### spatial data obtained from LANDFIRE for simulation in SyncroSim

processSpatialData <- function(mapzoneToKeep, runTag) {
  # Generate run-specific file paths ---------------------------------------
  
  # Directory to store cleaned rasters
  # Note that the working directory is prepended since SyncroSim needs absolute paths
  cleanRasterDirectory <- paste0(getwd(), "/data/clean/", runTag, "/")
  dir.create(cleanRasterDirectory, showWarnings = F)

  # Directory and prefix for FDIST binary rasters (spatial multipliers)
  transitionMultiplierDirectory <- paste0(cleanRasterDirectory, "transitionMultipliers/")
  dir.create(transitionMultiplierDirectory, showWarnings = F)

  # Clean Raster Paths
  mapzoneRasterPath <- paste0(cleanRasterDirectory, "MapZone.tif")
  evtRasterPath <- paste0(cleanRasterDirectory, "EVT.tif")
  evhRasterPath <- paste0(cleanRasterDirectory, "EVH.tif")
  evcRasterPath <- paste0(cleanRasterDirectory, "EVC.tif")
  fdistRasterPath <- paste0(cleanRasterDirectory, "FDIST.tif")
  vdistRasterPath <- paste0(cleanRasterDirectory, "VDIST.tif")
  stateClassRasterPath <- paste0(cleanRasterDirectory, "StateClass.tif")
  tilingRasterPath <- paste0(cleanRasterDirectory, "Tiling.tif")
  
  # Path for temporary files
  tempRasterPath <- paste0(cleanRasterDirectory, "temp.tif")

  # VDIST info for layerizing
  vdistInfoPath <- paste0(cleanRasterDirectory, "VDIST.csv")

  # Load non-spatial data --------------------------------------------------
  distCrosswalk <-   read_xlsx(distCrosswalkPath) %>%
    mutate(name = paste(d_type...2, d_severity...3, d_time...4, sep = " - ")) %>%
    dplyr::select(fdist = FDIST, vdist = VDIST, name)

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
      filename = tempRasterPath) %>%
    trimRaster(
      filename = mapzoneRasterPath
    )

  # Crop and mask data ----------------------------------------------------------

  # EVT
  evtRaster <-
    cropRaster(evtRaster, tempRasterPath, extent(mapzoneRaster)) %>%
    maskRaster(evtRasterPath, maskingRaster = mapzoneRaster)

  # EVC
  evcRaster <-
    cropRaster(evcRaster, tempRasterPath, extent(mapzoneRaster)) %>%
    maskRaster(evcRasterPath, maskingRaster = mapzoneRaster)

  # EVH
  evhRaster <-
    cropRaster(evhRaster, tempRasterPath, extent(mapzoneRaster)) %>%
    maskRaster(evhRasterPath, maskingRaster = mapzoneRaster)

  # FDIST
  fdistRaster <-
    cropRaster(fdistRaster, tempRasterPath, extent(mapzoneRaster)) %>%
    maskRaster(fdistRasterPath, maskingRaster = mapzoneRaster)

  # Convert disturbance to VDIST ------------------------------------------------

  # Split the FDIST raster into blocks to calculate the unique set of FDIST codes
  # present in the data without using excessive memory per thread
  # - We ignore NA (no data) as well as 0 (no disturbance)
  fdistLevels <- uniqueInRaster(fdistRaster, ignore = c(0, NA))

  # Check which fdist codes need to be reclassified to a different vdist code
  distReclassification <- distCrosswalk %>%
    filter(
      fdist %in% fdistLevels,
      fdist != vdist) %>%
    dplyr::select(-name) %>%
    as.matrix

  # Reclassify as necessary and save
  vdistRaster <- reclassify(fdistRaster,
                            distReclassification,
                            filename = vdistRasterPath)

  # Generate list of unique VDIST codes
  vdistLevels <- distCrosswalk %>%
    filter(fdist %in% fdistLevels) %>%
    pull(vdist) %>%
    unique %>%
    sort

  vdistNames <- distCrosswalk %>%
    dplyr::select(-fdist) %>%
    filter(vdist %in% vdistLevels) %>%
    arrange(vdist) %>%
    unique() %>%
    pull(name)

  write_csv(
    tibble(vdist = vdistLevels, name = vdistNames),
    vdistInfoPath)

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
      tempRasterPath,
      nx = tileCols)
  
  # Remove temp file
  unlink(tempRasterPath)
}

