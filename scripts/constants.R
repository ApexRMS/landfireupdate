### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### November 2020
### This a header file with global variables used by other scripts in this directory.
### Please check the paths and options set here prior to running the other scripts.

# Load Config -----------------------------------------------------------

config <- read_csv("config/config.csv") %>%
  pull(2, name = 1) %>%                      # Extract column 2, and use column 1 as names
  as.list()

# Parse Config ----------------------------------------------------------

if(!config$dataFolder %>% dir.exists)
  stop("Could not find input data folder. Please check config file.")

# Check that list of Map Zones to keep exist
if(!config$mapzonesToKeepPath %>% file.exists)
  stop("Can't find the list of Map Zones to keep. Please check the configuration file.")

# Check that the number of requested R threads is valid
if(config$nThreads %>% is.na) {
  nThreads <- future::availableCores()
  message("Using all available cores to build SyncroSim Library")
} else if (config$nThreads %>% as.integer %>% is.na | config$nThreads %>% as.integer < 1) {
  nThreads <- future::availableCores()
  warning("Invalid number of threads requested. Using all available cores to build SyncroSim Library")
}

# Check that the number of requested SyncroSim jobs is valid
if (config$ssimJobs %>% as.integer %>% is.na | config$ssimJobs %>% as.integer < 1) {
  ssimJobs <- 8
  warning("Invalid number of SyncroSim jobs requested. Using default value of 8 SyncroSim jobs to run the library after building.")
}

# Check that the number of tiling columns is valid
if(as.integer(config$tileCols) %>% is.na) {
  warning("Invalid number of tiling columns in config file. Using 20 columns.")
  config$tileCols <- 20
}

# Check SyncroSim installation directory
# - Use NULL in place of NA to use default installation location
if(config$ssimDir %>% is.na)
  config$ssimDir <- NULL

# Overall Options --------------------------------------------------------

# Which mapzones to process
mapzonesToKeep <- read_csv(config$mapzonesToKeepPath) %>% pull

# Tags used to identify the independent runs (one per Map Zone)
# They are used to generate names for output folders, etc
runTags <- str_c("Map Zone ", mapzonesToKeep)

# The name of the SyncroSim library to store the scenario, etc. in
runLibrary <- config$libraryName

# SyncroSim installation directory
# - If left blank (NA), convert to NULL to use default installation location
ssimDir <- config$ssimDir

# Log file path
logFilePath <- "run.log"
dir.create("log", showWarnings = F)

# Raw Inputs --------------------------------------------------------------

# Directory holding raw rasters
rawRasterDirectory <- str_c(config$dataFolder, "/raw/")
if(!rawRasterDirectory %>% dir.exists)
  stop("The raw data subfolder was not found within input data folder! Please see the README for how the input data folder should be organized.")

# Raw input rasters
mapzoneRawRasterPath <- str_c(rawRasterDirectory, "Map Zones.tif")
evtRawRasterPath     <- str_c(rawRasterDirectory, "EVT.tif")
evhRawRasterPath     <- str_c(rawRasterDirectory, "EVH.tif")
evcRawRasterPath     <- str_c(rawRasterDirectory, "EVC.tif")
fdistRawRasterPath   <- str_c(rawRasterDirectory, "FDIST.tif")

# Check that all input rasters are present
if(!mapzoneRawRasterPath %>% file.exists)
  stop("Could not find the raw Map Zone raster map. Please see the README for how input data should be organized.")
if(!evtRawRasterPath %>% file.exists)
  stop("Could not find the raw EVT raster map. Please see the README for how input data should be organized.")
if(!evhRawRasterPath %>% file.exists)
  stop("Could not find the raw EVH raster map. Please see the README for how input data should be organized.")
if(!evcRawRasterPath %>% file.exists)
  stop("Could not find the raw EVC raster map. Please see the README for how input data should be organized.")
if(!fdistRawRasterPath %>% file.exists)
  stop("Could not find the raw FDIST raster map. Please see the README for how input data should be organized.")

# Raw non-spatial data directory
rawNonSpatialDirectory <- str_c(rawRasterDirectory, "nonspatial/")
if(!rawNonSpatialDirectory %>% dir.exists)
  stop("The raw, non-spatial data subfolder was not found within input data folder! Please see the README for how the input data folder should be organized.")

# Load all non-spatial data
transitionTablePath <- str_c(rawNonSpatialDirectory, "Transition Table.csv")
allowedStatesPath   <- str_c(rawNonSpatialDirectory, "All Combinations.csv")
evcTablePath        <- str_c(rawNonSpatialDirectory, "EVC LUT.csv")
evhTablePath        <- str_c(rawNonSpatialDirectory, "EVH LUT.csv")
vdistTablePath      <- str_c(rawNonSpatialDirectory, "VDIST LUT.csv")
distCrosswalkPath   <- str_c(rawNonSpatialDirectory, "Disturbance Crosswalk.csv")
evcColorsPath       <- str_c(rawNonSpatialDirectory, "EVC Colors.csv")
evtColorTablePath   <- str_c(rawNonSpatialDirectory, "EVT Colors.csv")

if(!transitionTablePath %>% file.exists)
  stop("Could not find the file `Transition Table.csv`. This file outlines all transition rules. Please see the README for how input data should be organized.")
if(!allowedStatesPath %>% file.exists)
  stop("Could not find the file `All Combinations.csv`. This file provides every valid combination of EVT, EVC, EVH, and Map Zone. Please see the README for how input data should be organized.")
if(!distCrosswalkPath %>% file.exists)
  stop("Could not find the file `Disturbance Crosswalk.csv`. This file is used to convert FDIST codes to VDIST. Please see the README for how input data should be organized.")
if(!vdistTablePath %>% file.exists)
  stop("Could not find the file `VDIST LUT.csv`. This file is used to connect disturbance codes to names and disturbance types. Please see the README for how input data should be organized.")
if(!evcTablePath %>% file.exists)
  stop("Could not find the file `EVC LUT.csv`. This file is used to connect EVC codes to names. Please see the README for how input data should be organized.")
if(!evhTablePath %>% file.exists)
  stop("Could not find the file `EVH LUT.csv`. This file is used to connect EVH codes to names. Please see the README for how input data should be organized.")
if(!evcColorsPath %>% file.exists)
  stop("Could not find the file `EVC Colors.csv`. This file is used to connect EVC codes to colors for mapping. Please see the README for how input data should be organized.")
if(!evtColorTablePath %>% file.exists)
  stop("Could not find the file `EVT Colors.csv`. This file is used to connect EVT codes to colors for mapping. Please see the README for how input data should be organized.")

# Output Raster Options ---------------------------------------------------

# A tiling mask is produced to allow for Spatial Multiprocessing in SyncroSim
# The size of the rows is dictated by the size of the raster and memory constraints,
# but the number of columns can be set here
tileCols <- as.integer(config$tileCols)

# As part of the processing step, cleaned and cropped rasters are built.
# This is the directory they will be organized in. Each Map Zone will have it's
# own subdirectory within this folder and the absolute path must also be
# constructed for SyncroSim later.
cleanRasterDirectoryRelative <-  str_c(config$dataFolder, "/clean/")

# After the running the SyncroSim library, it is often useful to stitch the
# individual Map Zone raster maps back into the full Geo Area. This is the
# This is the directory the stitched raster maps will be stored in
stitchedRasterDirectory <- str_c(config$dataFolder, "/stitched/")

# Run Controls  -------------------------------------------------------

minimumIteration <- 1
maximumIteration <- 1
minimumTimestep <- 0
maximumTimestep <- 1

# SyncroSim Options -----------------------------------------------------
libraryName <- str_c("library/", runLibrary)
projectName <- config$projectName
subScenarioName <- "Shared Model Info"
scenarioNames <- runTags

# Set the owner for all SyncroSim objects
ssimOwner <- "LANDFIRE"

# Set descriptions for SyncroSim objects
libraryDescription     <- str_c("ST-Sim library for updating LANDFIRE EVC and ",
                                "EVH based on starting MZ, EVT, EVC, EVH and ",
                                "disturbances during the update period.")
projectDescription     <- str_c("Models for updating the ", config$projectName)
subScenarioDescription <- str_c("Sub Scenario used to define common model info ",
                                "used by all other scenarios in the project.")
scenarioDescriptions   <- str_c("Model scenario for Map Zone ", mapzonesToKeep)

# Set the number of concurrent jobs to in SyncroSim
ssimJobs <- as.integer(config$ssimJobs)
