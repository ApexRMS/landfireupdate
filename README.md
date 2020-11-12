# Landfire Update

Code to prepare data for Landfire Update using ST-Sim. This project consists of
a collection of scripts to (A) process and clean raw spatial data and (B) use
these data to generate a SyncroSim library to run the Landfire Update itself.

## Setup

### Dependencies

These scripts require working installations of R and SyncroSim, and were
developed on R version v4.0.3 and SyncroSim v2.0.22. Additionally the following
R packages must be installed: `rsyncrosim`, `tidyverse`, `raster`,  `furrr`,
`readxl`, `RODBC`, `rgdal`. The ST-Sim package must also be installed in
SyncroSim.

Additionally, you may have to install an [ODBC driver](https://en.wikipedia.org/wiki/Open_Database_Connectivity)
to connect to the database file using `RODBC`. A working installation of
Microsoft Access should provide this driver, but there are a number of free,
cross-platform alternatives available if needed.

### Data files

A number of data files are required that are not included on the GitHub repo due
to size constraints. These files have been compressed into a zip folder that can
be downloaded [here](https://s3.us-west-2.amazonaws.com/apexrms.com.public/Data/A236/LANDFIRE%20Update%20Data%20Files.zip).

The zip folder also contains the expected paths of the files, and so it is
recommended to extract the zip file directly into the root of this git repository.
In other words, the `data/` and `db/` subdirectories should be present in the same
directory as this README after extraction, not inside another folder (such as 
`LANDFIRE Update Data Files/`). 

## Configuration

It is recommended to configure the script to set run options, including which
map zones to process, how many cores to use, etc., as well as to ensure that
input file paths are correctly set.

The `scripts/constants.R` script is the only file that needs to be reviewed and
edited to configure a run. Below is a breakdown of the organization of this file.

### Overall Run Options

These options set overarching options specific to the run.

The `runTag` variable is used to identify the run in some way and can be used to
indicate, for example, which subset of Map Zones are being analyzed in the
current run. In addition to being used to organize output files, the run tag is
also used to name the SyncroSim scenario. Accordingly, using unique run tags for
a sequence of runs can be used to generate a collection of scenarios within a
single SyncroSim library.  

To store the SyncroSim scenarios in a different SyncroSim library, for example
when testing, you can change the `runLibrary` variable.

> :warning: Output files from past runs with the same run tag and library name
> will be overwritten!

The `mapzoneToKeep` variable is used to select the Map Zone that should be
extracted and processed in the current run. Currently, this variable must
identify a single Map Zone, but we plan to add support for processing
collections of Map Zones together in future updates.

A number of the raster pre-processing operations have been parallelized to speed
up run time. The number of threads to be used for these steps can be set using
the `nThreads` variable. This is not to be confused with the `ssimJobs` in the
**SyncroSim Options** section below that is used to set the default maximum
number of jobs in the generated SyncroSim library. 

### Raw Inputs

This section is used to set the paths of input files. If the data files were
unzipped directly into this repository, the names and paths should be correct,
but it is worthwhile to double check that the files are in the correct
locations. If new inputs files are to be added, these variables should be
updated to reflect their file names and paths.

### Database Table Names

The names of SQL tables accessed from the database are listed here. As new
versions of these tables become available, both the database filename (listed in
the `Raw Inputs` section) and the table names must be updated as necessary.

### Cleaned Raster Options

These variables are used to define the naming scheme of the cleaned rasters
using the run tag. Most of these variables can safely be left alone.

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
can be set here, including the file owner and descriptions.

`ssimJobs` can also be set here specify the default maximum number of jobs in
the generated SyncroSim library. This is not to be confused with the `nThreads`
variable in the **Overall Run Options** section above that determines the
number of threads R will spawn while processing the raw rasters for SyncroSim.

## Running the Update

Begin by opening the `LANDFIRE Update.Rproj` R project file to ensure the
correct working directory is set in RStudio. Next, open the
`scripts/processSpatialData.R` script and either run line-by-line or press the
`source` button in the top right corner of the file editor pane.

This script is responsible for processing the raw input rasters, including
cropping and masking down to the chosen Map Zone, converting fDIST maps to
vDIST, and layerizing the disturbance map for SyncroSim.

Once this is done, open the `scripts/buildSsimLibrary.R` script and run as
before. This script uses the cleaned rasters and rules from the database to
generate a SyncroSim library file to run the update.

Finally, open the generated SyncroSim library, which will be stored in the
`library/` subdirectory. Select the `NW GeoArea` project, and the scenario you
would like to run and press `run` in the main toolbar.
