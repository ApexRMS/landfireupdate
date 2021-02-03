### LANDFIRE Project
### APEX RMS - Valentin Lucet and Shreeram Senthivasan
### September 2020
### The function defined in this script is used to clean and pre-process raw
### spatial data obtained from LANDFIRE for simulation in SyncroSim

processSpatialData <- function(mapzoneToKeep, runTag) {
  # Generate run-specific file paths ---------------------------------------
  
  # Directory to store cleaned rasters
  # Note that the working directory is prepended since SyncroSim needs absolute paths
  cleanRasterDirectory <- str_c(getwd(), "/", cleanRasterDirectoryRelative, runTag, "/")
  dir.create(cleanRasterDirectory, recursive = T, showWarnings = F)

  # Directory and prefix for FDIST binary rasters (spatial multipliers)
  transitionMultiplierDirectory <- str_c(cleanRasterDirectory, "transitionMultipliers/")
  dir.create(transitionMultiplierDirectory, showWarnings = F)

  # Clean Raster Paths
  mapzoneRasterPath <- str_c(cleanRasterDirectory, "MapZone.tif")
  evtRasterPath <- str_c(cleanRasterDirectory, "EVT.tif")
  evhRasterPath <- str_c(cleanRasterDirectory, "EVH.tif")
  evcRasterPath <- str_c(cleanRasterDirectory, "EVC.tif")
  fdistRasterPath <- str_c(cleanRasterDirectory, "FDIST.tif")
  vdistRasterPath <- str_c(cleanRasterDirectory, "VDIST.tif")
  stateClassRasterPath <- str_c(cleanRasterDirectory, "StateClass.tif")
  tilingRasterPath <- str_c(cleanRasterDirectory, "Tiling.tif")
  
  # Path for temporary files
  tempRasterPath <- str_c(cleanRasterDirectory, "temp.tif")

  # VDIST info for layerizing
  vdistInfoPath <- str_c(cleanRasterDirectory, "VDIST.csv")

  # Load non-spatial data --------------------------------------------------
  distCrosswalk <-   read_csv(distCrosswalkPath) %>%
    mutate(name = paste(d_type, d_severity, d_time, sep = " - ")) %>%
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
  
  # Crop to extent if that option is enabled
  if(cropToExtent) {
    
    # Check that the extent is completely contained by the chosen Map Zone
    if(any(mapzoneRaster %>% xmin > cropExtent %>% xmin,
           mapzoneRaster %>% ymin > cropExtent %>% ymin,
           mapzoneRaster %>% xmax < cropExtent %>% xmax,
           mapzoneRaster %>% ymax < cropExtent %>% ymax))
      stop("The chosen crop extent does not lie entirely with the chosen Geo Area. Please change the Geo Area or crop extent, or disable `cropToExtent`.")
      
    # This code assumes that the crop extent is fairly small
    # This call of raster::crop will be very slow for large extents
    mapzoneRaster <-
      crop(mapzoneRaster, cropExtent, filename = mapzoneRasterPath, overwrite = T)
    
    if(!mapzoneToKeep %in% uniqueInRaster(mapzoneRaster))
      stop("The chosen Map Zone does not overlap with the crop extent. Please change the Map Zone or crop extent, or disable `cropToExtent`.")
  }

  # Mask and trim Map Zone map by the chosen Map Zone
  # - Note that trimRaster also saves the raster to the cleaned raster directory
  mapzoneRaster <-
    maskByMapzone(
      inputRaster = mapzoneRaster,
      maskValue = mapzoneToKeep,
      filename = tempRasterPath) %>%
    trimRaster(
      filename = mapzoneRasterPath
    )
  
  # In preparation for using the FDIST layer as a mask, we need to construct a
  # matrix describing which cells in the FDIST we wish to assign a value of NA
  # - This will include 0 (no disturbance) but also any TSD codes we wish to
  #   remove from the analysis
  # - The format of this matrix is dictated by the `raster::reclassify()` function
  # - Note: the final digit of the FDIST code is just the TSD code
  fdistToRemove <-
    read_csv(distCrosswalkPath) %>%
    filter((FDIST %% 10) %in% tsdToRemove) %>% # Note: (x %% 10) is just the last digit of x
    pull(FDIST) %>%
    as.integer()
  
  fdistReclassification <-
    matrix(
      c(0L,
        fdistToRemove,
        rep(NA_integer_, length(fdistToRemove) + 1)),
      ncol = 2)
    

  # Crop and mask Disturbance map down to the Map Zone of interest
  # Reclassify 0 (no disturbance) as NA for masking other maps
  # Finally trim down to only include relevant cells
  fdistRaster <-
    cropRaster(fdistRaster, tempRasterPath, extent(mapzoneRaster)) %>%
    maskRaster(fdistRasterPath, maskingRaster = mapzoneRaster) %>%
    reclassify(
      rcl = fdistReclassification,
      filename = tempRasterPath,
      overwrite = TRUE) %>%
    trimRaster(filename = fdistRasterPath)
  
  # Crop Map Zone map down to only cells that have been disturbed and then mask
  # by the disturbance map
  mapzoneRaster <-
    cropRaster(mapzoneRaster, tempRasterPath, extent(fdistRaster)) %>%
    maskRaster(mapzoneRasterPath, fdistRaster)
  
  # Crop and mask data ----------------------------------------------------------

  # At this point, both the FDIST and Map Zone maps have been cropped and masked
  # to only the disturbed cells in the region of interest. Either could be used
  # to crop and mask the remaining raster maps. Here the Map Zone map is used.

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
                            filename = vdistRasterPath,
                            overwrite = TRUE)

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
      tileSize = tileSize)
  
  # Remove temp file
  unlink(tempRasterPath)
}

