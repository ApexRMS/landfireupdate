# Landfire Update

Code to prepare data for Landfire Update using ST-Sim. This project consists of
a collection of scripts to (A) process and clean raw spatial data and (B) use
these data to generate a SyncroSim library to run the Landfire Update itself.

## Setup

### Dependencies

These scripts require working installations of R and SyncroSim, and were
developed on R version v4.0.3 and SyncroSim v2.0.23. Additionally the following
R packages must be installed: `rsyncrosim`, `tidyverse`, `raster`,  `furrr`,
`readxl`, `rgdal`, `logr`. The ST-Sim package (v3.2.25) must also be installed in
SyncroSim. The instructions to run the script assume you will be using [RStudio](https://rstudio.com/),
however, this is not a strict requirement.

### Data files

A number of data files are required that are not included on the GitHub repo due
to size constraints. These files have been compressed into a zip folder that can
be downloaded [here](https://s3.us-west-2.amazonaws.com/apexrms.com.public/Data/A236/LANDFIRE%20Update%20Data%20Files%20-%202020-11-24.zip).
Please note that this file is quite large (~1.3GB).

The zip folder also contains the expected paths of the files, and so it is
recommended to extract the zip file directly into the root of this git
repository. In other words, the `data/` and `library/` subdirectories should be
present in the same directory as this README after extraction, not inside
another folder (such as `LANDFIRE Update Data Files/`).

This archive also includes an example SyncroSim library (`library/LANDFIRE Update.ssim`)
generated using this repository for Map Zone 19. This example library can be
run directly in SyncroSim without running any of the R scripts. The default
configuration for the R scripts (described below) should reproduce this library
exactly.

## Configuration

It is recommended to configure the script to set run options, including which
map zones to process, how many cores to use, etc., as well as to ensure that
input file paths are correctly set.

The `scripts/constants.R` script is the only file that needs to be reviewed and
edited to configure a run. Below is a breakdown of the organization of this file.

### Overall Options

These options set overarching options specific to the run.

The `mapzonesToKeep` variable is used to select the Map Zones that should be
extracted and processed in the current run. This should be a vector of numbers.
To identify which Map Zones are present in a Geo Area, consider using the
`uniqueInRaster()` function defined in `scripts/rasterFunctions.R`.

The `runTags` variable is used to identify the individual runs, and should be
the same length as `mapzonesToKeep`. In addition to being used to organize
output files, the run tag is also used to name the SyncroSim scenario.

To store the SyncroSim scenarios in a different SyncroSim library, for example
when testing, you can change the `runLibrary` variable.

> :warning: Output files from past runs with the same run tag and library name
> will be overwritten!

A number of the raster pre-processing operations have been parallelized to speed
up run time. The number of threads to be used for these steps can be set using
the `nThreads` variable. This is not to be confused with the `ssimJobs` in the
**SyncroSim Options** section below that is used to set the default maximum
number of jobs in the generated SyncroSim library. 

The `ssimDir`is used to indicate the installation directory for SyncroSim if it
is not in the default location. This is primarily included for Linux users.
Leave this values as `NULL` to use the default isntallation directory.

Finally `logFilePath` is used to name the log file that will be produced during
the run.

### Raw Inputs

This section is used to set the paths of input files. If the data files were
unzipped directly into this repository, the names and paths should be correct,
but it is worthwhile to double check that the files are in the correct
locations. If new inputs files are to be added, these variables should be
updated to reflect their file names and paths.

### Output Raster Options

One of the cleaned rasters is a mask that is used to split the remaining rasters
down into manageable rectangular chunks (or tiles) for spatial multiprocessing
in SyncroSim. The `raster::blockSize()` function is used to decide the number
of rows to split the rasters into, but the number of columns can be manually set
using the `tileCols` variable. 

### Run Controls

These variables are used to set the SyncroSim run controls, including the start
and end timesteps and the number of iterations.

### SyncroSim Options

More detailed information about the SyncroSim library, project, and scenarios
can be set here, including the file owner and descriptions. Note that
`scenarioNames` and `scenarioDescriptions` both must have the same length as
`mapzonesToKeep`.

`ssimJobs` can also be set here specify the default maximum number of jobs in
the generated SyncroSim library. This is not to be confused with the `nThreads`
variable in the **Overall Run Options** section above that determines the
number of threads R will spawn while processing the raw rasters for SyncroSim.

## Running the Update

Begin by opening the `LANDFIRE Update.Rproj` R project file to ensure the
correct working directory is set in RStudio. Next, open the
`batchProcess.R` script and either run line-by-line or press the
`source` button in the top right corner of the file editor pane of RStudio.

This script is responsible for processing the raw input rasters, including
cropping and masking down to the chosen Map Zone, converting fDIST maps to
vDIST, and layerizing the disturbance map for SyncroSim.

Once this is done, open the `scripts/buildSsimLibrary.R` script and run as
before. This script uses the cleaned rasters and rules from the database to
generate a SyncroSim library file to run the update.

Finally, open the generated SyncroSim library using the SyncroSim UI. 
This file will be stored in the `library/` subdirectory. Select the 
`NW GeoArea` project, and the scenario you would like to run and press 
`run` in the main toolbar. Note that, depending on your computer configuration,
you may need to change the maximum number of [multiprocessing](http://docs.syncrosim.com/how_to_guides/modelrun_multiproc.html) jobs.
As an example of requirements, Map Zone 19 (120 million cells) was run 
with a maximum of 5 multiprocessing jobs on a Windows 2019 Server with 
16 virtual cores and 128GB of RAM.  The run took approximately 3 hours 
and used up to 80% of available memory.

Results can be viewed directly in the graphical user interface (GUI) by 
creating [Charts](http://docs.syncrosim.com/how_to_guides/results_chart_window.html)
and [Maps](http://docs.syncrosim.com/how_to_guides/results_map_window.html). 
You can also export tabular and map data to be viewed externally.

NOTE that using the GUI is optional and the scenario can also be run directly 
through the SyncroSim commandline or by using the rsyncrosim package for R.
