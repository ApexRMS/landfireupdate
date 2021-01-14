### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### November 2020
### This a header file that defines memory-safe raster functions that are optimized
### for large data files that cannot easily be held in memory.

# A memory-safe function to convert a raster with multiple values (map zones)
# into a mask for a single value (map zone)
# - Requires an output filename, slower than raster::mask for small rasters
maskByMapzone <- function(inputRaster, maskValue, filename){
  # Integer mask values save memory
  maskValue <- as.integer(maskValue)

  # Let raster::blockSize() decide appropriate blocks to break the raster into
  blockInfo <- blockSize(inputRaster)

  # Generate empty raster with appropriate dimensions
  outputRaster <- raster(inputRaster)

  # Calculate mask and write to output block-by-block
  outputRaster <- writeStart(outputRaster, filename, overwrite=TRUE)

  for(i in seq(blockInfo$n)) {
    blockMask <-
      getValuesBlock(inputRaster, row = blockInfo$row[i], nrows = blockInfo$nrows[i]) %>%
      `==`(maskValue) %>%
      if_else(true = maskValue, false = NA_integer_)
    outputRaster <- writeValues(outputRaster, blockMask, blockInfo$row[i])
  }

  outputRaster <- writeStop(outputRaster)

  return(outputRaster)
}

# A memory-safe implementation of `raster::unique()` optimized for large rasters
# - ignore are the levels to exclude from the output
uniqueInRaster <- function(inputRaster, ignore = NA) {
  # Choose number of blocks to split rasters into when processing to limit memory
  blockInfo <- blockSize(inputRaster)

  # Calculate unique values in each block
  map(
    seq(blockInfo$n),
    ~ unique(getValuesBlock(inputRaster,
                            row   = blockInfo$row[.x],
                            nrows = blockInfo$nrows[.x]))) %>%
  # Consolidate values from each block
  flatten_dbl %>%
  unique() %>%
  # Exclude values to ignore
  `[`(!. %in% ignore) %>%
  return

}

# A memory-safe implementation of raster::crop() optimized for large rasters
# - Requires an extent object, unlike raster::crop()
# - Requires an output filename, slower than raster::crop for small rasters
cropRaster <- function(inputRaster, filename, outputExtent) {
  # Create empty raster to hold output
  outputRaster <-  raster(inputRaster) %>%
    crop(outputExtent)

  # Calculate offset from original
  offsetAbove <- round((ymax(extent(inputRaster)) - ymax(outputExtent)) / res(inputRaster)[2])
  offsetLeft <-  round((xmin(outputExtent) - xmin(extent(inputRaster))) / res(inputRaster)[1])

  ## Split output into manageable chunks and fill with data from input
  blockInfo <- blockSize(outputRaster)

  outputRaster <- writeStart(outputRaster, filename, overwrite=TRUE)

  for(i in seq(blockInfo$n))
    outputRaster <-
    writeValues(outputRaster,
                getValuesBlock(inputRaster,
                               row   = blockInfo$row[i] + offsetAbove,
                               nrows = blockInfo$nrows[i],
                               col   = offsetLeft + 1,
                               ncols = ncol(outputRaster)),
                blockInfo$row[i])

  outputRaster <- writeStop(outputRaster)

  return(outputRaster)
}

# Function to convert a vector, etc. of number into a multiplicative mask
# - Replaces all values with 1, NA remains as NA
# - Assumes no negative numbers (specifically, no -1)
maskify <- function(x) {
  x <- x + 1L # Used to avoid divide by zero errors, this is why -1 is not acceptable
  return(x / x)
}

# A memory safe implementation of raster::mask() optimized for large rasters
# - Requires an output filename, slower than raster::mask for small rasters
# - Input and mask rasters must have same extent (try cropRaster() if not)
maskRaster <- function(inputRaster, filename, maskingRaster){
  # Create an empty raster to hold the output
  outputRaster <-  raster(inputRaster)

  ## Split output into manageable chunks and fill with data from input
  blockInfo <- blockSize(outputRaster)

  ## Calculate mask and write to output block-by-block
  outputRaster <- writeStart(outputRaster, filename, overwrite=TRUE)

  # Each block of the mask raster is converted into a multiplicative mask
  # and multiplied with the corresponding block of the input raster
  for(i in seq(blockInfo$n)) {
    maskedBlock <-
      getValuesBlock(maskingRaster, row = blockInfo$row[i], nrows = blockInfo$nrows[i]) %>%
      maskify %>%
      `*`(getValuesBlock(inputRaster, row = blockInfo$row[i], nrows = blockInfo$nrows[i]))
    outputRaster <- writeValues(outputRaster, maskedBlock, blockInfo$row[i])
  }

  outputRaster <- writeStop(outputRaster)

  return(outputRaster)
}

# A function to crop rasters down to remove borders filled with only NA's
# - Uses binary search to quickly process large rasters with large empty borders
# - Requires an output filename, slower than raster::trim for small rasters
# - maxBlockSizePower is an integer that will be used to calculate the max number
#   of rows / cols to load into memory. Specifically 2^maxBlockSizePower rows and
#   cols will be loaded at most at a time
trimRaster <- function(inputRaster, filename, maxBlockSizePower = 11){
  # Reading portions of large rasters that can't be held in memory is slow, so
  # we want to minimize the number of reads as we identify how much we can trim
  # off each of the four sides of the raster

  # One simplistic approach is to check large blocks at a time until a block
  # with non-NA data is found. Next halve the size of the search block and
  # continue searching. Keep halving the the width of the search block until you
  # find the single first column with data. This is effectively a binary search
  # once the first (largest) block with non-NA data is found.

  # Setup --------------------------------------------------------------------
  # Decide how to split input into manageable blocks
  maxBlockSize <- 2^(maxBlockSizePower)
  # Make sure max block size is smaller than the number of columns and rows
  while(ncol(inputRaster) < maxBlockSize | nrow(inputRaster) < maxBlockSize){
    maxBlockSize = maxBlockSize / 2
    maxBlockSizePower = maxBlockSizePower - 1
  }
  descendingBlockSizes <- 2^((maxBlockSizePower-1):0)

  # Initialize counters
  trimAbove <- 1
  trimBelow <- nrow(inputRaster) + 1
  trimLeft  <- 1
  trimRight <- ncol(inputRaster) + 1

  # Top ----------------------------------------------------------------------
  # Search for the first block from the top with data
  while(
    getValuesBlock(inputRaster,
                   row = trimAbove,
                   nrows = maxBlockSize) %>%
    is.na %>%
    all
  )
    trimAbove <- trimAbove + maxBlockSize

  # Now do a binary search for the first row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster, row = trimAbove, nrows = i) %>% is.na %>% all)
      trimAbove <- trimAbove + i
  
  # Pad if possible
  trimAbove <- max(trimAbove - 2, 0)

  # Bottom  -------------------------------------------------------------------
  # Repeat from the bottom up, first finding a block that is not all NA
  while(
    getValuesBlock(inputRaster,
                   row = trimBelow - maxBlockSize,
                   nrows = maxBlockSize) %>%
    is.na %>%
    all
  )
    trimBelow <- trimBelow - maxBlockSize

  # Binary search for last row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster, row = trimBelow - i, nrows = i) %>% is.na %>% all)
      trimBelow <- trimBelow - i

  # Calculate height of the trimmed raster
  outputRows <- trimBelow - trimAbove

  # Left  --------------------------------------------------------------------
  # Search for the first block from the left with data
  while(
    getValuesBlock(inputRaster,
                   col   = trimLeft,
                   ncols = maxBlockSize,
                   row   = trimAbove,
                   nrows = outputRows) %>%
    is.na %>%
    all
  )
    trimLeft <- trimLeft + maxBlockSize

  # Now do a binary search for the first row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster,
                      col   = trimLeft,
                      ncols = i,
                      row   = trimAbove,
                      nrows = outputRows) %>%
       is.na %>%
       all)
      trimLeft <- trimLeft + i
  
  # Pad if possible
  trimLeft <- max(trimLeft - 2, 0)

  # Right  --------------------------------------------------------------------
  # Repeat for the first block from the right with data
  while(
    getValuesBlock(inputRaster,
                   col   = trimRight - maxBlockSize,
                   ncols = maxBlockSize,
                   row   = trimAbove,
                   nrows = outputRows) %>%
    is.na %>%
    all
  )
    trimRight <- trimRight - maxBlockSize

  # Now do a binary search for the first row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster,
                      col   = trimRight - i,
                      ncols = i,
                      row   = trimAbove,
                      nrows = outputRows) %>%
       is.na %>%
       all)
      trimRight <- trimRight - i

  # Calculate width of the trimmed raster
  outputCols <- trimRight - trimLeft

  # Crop  ---------------------------------------------------------------------
  
  # Don't crop if there is nothing to crop
  if(trimAbove == 1 & trimLeft == 1 & trimBelow == nrow(inputRaster) + 1 & trimRight == ncol(inputRaster) + 1)
    return(writeRaster(inputRaster, filename = filename, overwrite = T))

  # Convert trim variables to x,y min,max
  outXmin <- xmin(extent(inputRaster)) + trimLeft * res(inputRaster)[1]
  outXmax <- outXmin + outputCols *  res(inputRaster)[1]
  outYmax <- ymax(extent(inputRaster)) - trimAbove * res(inputRaster)[2]
  outYmin <- outYmax - outputRows * res(inputRaster)[2]

  return(
    cropRaster(
      inputRaster,
      filename,
      extent(c(xmin = outXmin,
               xmax = outXmax,
               ymin = outYmin,
               ymax = outYmax))
      )
  )
}

# Function to create and save a binary raster given a non-binary raster and the
# value to keep
# - Used to binarize disturbance maps for use as spatial multipliers in SyncroSim
saveDistLayer <- function(distValue, distName, fullRaster, transitionMultiplierDirectory) {
  writeRaster(
    layerize(fullRaster, classes = distValue),
    paste0(transitionMultiplierDirectory, distName, ".tif"),
    overwrite = TRUE
  )
}

# Function to generate a tiling mask given template
# - To avoid holding the entire raster in memory, the raster is written directly
#   to file row-by-row. This way only one row of the tiling needs to be held in
#   memory at a time. This is also why the number of rows (and implicitly the size
#   of a given row) cannot be chosen manually
# - minProportion is used to determine the size threshold for consolidating tiles
#   that are too small into neighboring tiles. Represented as a porportion of a
#   full tile
tilize <- function(templateRaster, filename, tempfilename, nx, minProportion = 0.2) {
  # Calculate recommended block size of template
  blockInfo <- blockSize(templateRaster)
  
  # Check that the blockSize is meaningful
  # - This should only matter for very small rasters, such as in test mode
  if(max(blockInfo$nrows) == 1)
    blockInfo <- list(row = 1, nrows = nrow(templateRaster), n = 1)
  
  # Extract number of rows
  ny <- blockInfo$n
  
  # Calculate dimensions of each tile
  tileHeight <- blockInfo$nrows[1]
  tileWidth <- ceiling(ncol(templateRaster) / nx)

  # Generate a string of zeros the width of one tile
  oneTileWidth <- rep(0, tileWidth)

  # Generate one line of one row, repeat to the height of one row
  oneRow <-
    as.vector(vapply(seq(nx), function(i) oneTileWidth + i, FUN.VALUE = numeric(tileWidth))) %>%
    `[`(1:ncol(templateRaster)) %>% # Trim the length of one row to fit in template
    rep(tileHeight)

  # Write an empty raster with the correct metadata to file
  tileRaster <- raster(templateRaster)

  # Write tiling to file row-by-row
  tileRaster <- writeStart(tileRaster, tempfilename,  overwrite=TRUE)
  for(i in seq(blockInfo$n)) {
    if(blockInfo$nrows[i] < tileHeight)
      oneRow <- oneRow[1:(ncol(tileRaster) * blockInfo$nrows[i])]
    #browser()
    tileRaster <- writeValues(tileRaster, oneRow, blockInfo$row[i])
    oneRow <- oneRow + nx
  }
  tileRaster <- writeStop(tileRaster)

  # Mask raster by template
  tileRaster <-
    maskRaster(tileRaster, filename, maskingRaster = templateRaster)
  
  return(tileRaster)
}