# Landfire Update

Code to prepare data for LANDFIRE Update using ST-Sim. This project consists of
a collection of scripts to process and clean raw spatial data and use these data
to generate a SyncroSim library to run the LANDFIRE Update itself.

## Setup

### Dependencies

These scripts require working installations of R and SyncroSim, and were
developed on R version v4.0.3 and SyncroSim v2.0.23. Additionally the following
R packages must be installed: `rsyncrosim`, `tidyverse`, `raster`,  `furrr`,
`rgdal`, `logr`. The ST-Sim package (v3.2.25) must also be installed in
SyncroSim. The instructions to run the script assume you will be using [RStudio](https://rstudio.com/),
however, this is not a strict requirement.

### Data files

A number of data files are required that are not included on the GitHub repo due
to size constraints. These files have been compressed into a zip folder that can
be downloaded [here](https://s3.us-west-2.amazonaws.com/apexrms.com.public/Data/A236/LANDFIRE%20Update%20Data%20Files%20-%202020-12-26.zip).
Please note that this file is quite large (~1.3GB).

The zip folder also contains the expected paths of the files, and so it is
recommended to extract the zip file directly into the root of this git
repository. In other words, the `data/` and `library/` subdirectories should be
present in the same directory as this README after extraction, not inside
another folder (such as `LANDFIRE Update Data Files/`).

This archive also includes an example SyncroSim library (`library/NW Geo Area Update.ssim`)
generated using this repository for Map Zone 19. This example library can be
run directly in SyncroSim without running any of the R scripts. The default
configuration (described below) should reproduce this library exactly.

## Configuration

The run can be configured by editing the `config/config.csv` file. This
spreadsheet consists of three columns: `Variable` which indicates the name of
the option being set; `Value` which is used to actually set the option; and
`Comment` which provides additional information about the option. The `Value`
column is the only column that needs to be modified during configuration. Editing
the `Comment` column is safe, but the `Variable` column should not be edited
unless all the R scripts are updated accordingly.

A few variables are also surrounded by `*`. These are not read by the script and
are used as section headers to organize the remaining variables. A description
of the configuration variables organized by these sections follows.

### Input Data Paths

Two data paths must be provided to the configuration. The first, `dataFolder`,
should be a valid path point to a folder containing all the necessary input data.
There is a strict naming scheme and organization for the files within this
directory. See the **Input Data Structure** Section below for more information.

The other input file, `mapzonesToKeepPath`, should be a CSV file with a single
column listing each Map Zone to include in the run on a separate line. Note that
a header line is required. The `config/Map Zone Lists/` folder includes two
examples, one for running just Map Zone 19 and another for running the entire
NW Geo Area.

### SyncroSim Object Names

These are the names of the SyncroSim library and project that will be built by
the R scripts. It is recommended to include the name of the Geo Area in both
of these names.

### Parallel Processing Options

This workflow is parallelized at two different stages: building the SyncroSim
library and running the SyncroSim library. The `nThreads` configuration variable
determines the number of cores R should use while building the library, while
`ssimJobs` determines the number of cores SyncroSim should use while running the
library.

These variables are set independently as these two overarching tasks have very
different resource requirements and accordingly benefit from different numbers
of cores on most machines. In particular, building the library can only ever use
as many cores as there are unique disturbance events in a single Map Zone (about
20, on average) and makes most efficient use of about 8 cores. In contrast,
running the library will almost always benefit from more cores, but this task
is strongly memory limited. Accordingly, setting too many jobs here can lead to
Out-of-Memory errors. Please refer to the **Suggested Configuration** Section
below for suggestions on setting `ssimJobs`.

Finally, `tileCols` can be used to decide how finely the raster maps are split
up for the spatial multiprocessing in SyncroSim. Higher values will decrease
memory usage in SyncroSim (to a point), but will generally increase run time.

### Testing Options

These options can be used to crop the output raster maps down to a smaller fixed
extent, primarily to speed up runs for testing. This feature can be enabled by
setting `cropToExtent` to 1. Set this variable to 0 to disable cropping to
extent. When this feature is enabled, an extent will be read from the CSV file
at `cropExtentPath`. See the example crop extent CSV for Map Zone 19, 
`config/Crop Extents/MZ 19 Example.csv` for the proper format. Note that all
Map Zones to be kept must at least partially overlap the chosen extent.

### SyncroSim Installation Options

`ssimDir` can be used to explicitly set the location of the SyncroSim
installation. This is primarily of use on Linux machines. Leave this cell empty
to use the default installation directory.

## Suggested Configuration

The suggested run configuration will depend on the available compute resources.
Below is a table outlining some general recommendations and the corresponding
expected run times.


| Min. Processors | Min. Memory | SyncroSim Jobs | Tiling Columns | Approx. Run Time for NW Geo Area | Approx. Run Time for Map Zone 19 |
|----------------:|------------:|---------------:|---------------:|---------------------------------:|---------------------------------:|
|               2 |        32GB |              1 |             20 |                       40 - 48 d  |                        80 - 95 h |
|               4 |        64GB |              2 |             20 |                       20 - 24 d  |                        40 - 50 h |
|               4 |       100GB |              4 |             20 |                       10 - 12 d  |                        20 - 25 h |
|               8 |       160GB |              8 |             20 |                         5 - 6 d  |                        11 - 12 h |
|              16 |       256GB |             16 |             10 |                         2 - 3 d  |                            ~ 5 h |
|              32 |       512GB |             32 |             10 |                           ~24 h  |                            ~ 2 h |
|              64 |      1024GB |             64 |             10 |                           ~16 h  |                          ~ 1.5 h |

To use this table, find the last row for which you have at least the minimum
number of available processors *and* minimum amount of memory. Set the number of
SyncroSim jobs (`ssimJobs`) and tiling columns (`tileCols`) accordingly in the
config file. The remaining two columns provide ranges for the expected run times.

## Running the Update

Begin by opening the `LANDFIRE Update.Rproj` R project file to ensure the
correct working directory is set in RStudio. Next, open the `batchProcess.R` 
script and either run line-by-line or press the `source` button in the top right
corner of the file editor pane of RStudio.

This script is responsible for processing the raw input raster maps, including
cropping and masking down to the chosen Map Zone, converting fDIST maps to
vDIST, and layerizing the disturbance map for SyncroSim. The script then uses
these cleaned rasters to generate a SyncroSim library file to run the update.

Next, open the generated SyncroSim library using the SyncroSim UI. This file
will be stored in the `library/` subdirectory. Select the `NW Geo Area` project,
(or the `projectName` you chose in the configuration) and the scenario you would
like to run and press `run` in the main toolbar. Note that, depending on your
computer configuration, you may need to change the maximum number of
[multiprocessing](http://docs.syncrosim.com/how_to_guides/modelrun_multiproc.html)jobs.
As an example of requirements, the NW Geo Area (consisting of 12 Map Zones) was
run with a maximum of 32 multiprocessing jobs on a Linux server with 64 virtual
cores and 512GB of RAM and took just under 21 hours.

Results can be viewed directly in the graphical user interface (GUI) by 
creating [Charts](http://docs.syncrosim.com/how_to_guides/results_chart_window.html)
and [Maps](http://docs.syncrosim.com/how_to_guides/results_map_window.html). 
You can also export tabular and map data to be viewed externally.

Finally, to reconstruct EVC and EVH raster maps for the entire Geo Area from the
SyncroSim results, run the `reconstructGeoArea.R` script as before. The full maps
will be stored in the `stitched/` subdirectory of the input `dataFolder` you set
in the configuration.

NOTE that using the GUI is optional and the scenario can also be run directly 
through the SyncroSim commandline or by using the rsyncrosim package for R.

## Input Data Structure

To generalize the workflow for running multiple Geo Areas with minimal
reconfiguration, the scripts have strict requirements for how input files are
named and organized.

The input files for each Geo Area must be stored in its own folder, preferably
in the `data/` directory with a name that indicates which Geo Area the folder is
for. For example, the example data archive linked in the **Setup** section 
contains the input files for the NW Geo Area and are accordingly are held in
`data/NW/`.

Within this directory, there must be a subdirectory named `raw/`. This is used
to separate the raw inputs from the final clean and stitched raster maps that
will be generated and stored in their respective subdirectories.

Within the `raw/` subdirectory, the raw raster maps of the Map Zones, EVC, EVH,
EVT, and fDIST must be present and named `Map Zones.tif`, `EVC.tif`, `EVH.tif`,
`EVT.tif`, and `FDIST.tif` respectively.

Also required within the `raw/` subdirectory is the `nonspatial/` subdirectory,
which includes Geo-Area-specific data files that are not stored as maps. This
folder requires a spreadsheet of all transition rules, `Transition Table.csv`,
and a color mapping for EVT codes present in the Geo Area, `EVT Colors.csv`.

Altogether, the folder should have the following structure:

```
Geo Area Name
|
└───raw
    │   Map Zones.tif
    │   EVC.tif
    │   EVH.tif
    │   EVT.tif
    │   FDIST.tif
    │
    └───nonspatial
        │   Transition Table.csv
        │   EVT Colors.csv

```

Please see the example data files linked in the **Setup** section for details on
the exact information expected in each file.