# Landfire Update

This project contains code to update LANDFIRE raster maps of Existing Vegetation
Cover (EVC) and and Existing Vegetation Height (EVH) using Fuel Disturbance
(FDist) data and transition rules. This process is broken into three principal
tasks: generating a SyncroSim library that organizes these inputs and transition
rules by Map Zone; running the SyncroSim library to forecast these new
vegetation parameters by Map Zone; and reconstructing EVC and EVH maps for the
entire Geo Area from the maps generated for each Map Zone.

Below are the instructions to setup, configure, and run the code.

## Table of Contents

#### [Setup](#Setup-1)

#### [Configuration](#Configuration-1)
 
#### [Suggeted Configuration](#Suggested)
 
#### [Running the Update](#Running)
 
#### [Input Data Structure](#InputStructure)
 
#### [Data Dictionary](#Dictionary)

## Setup

### Dependencies

These scripts require working installations of R and SyncroSim, and were
developed on R version v4.0.3 and SyncroSim v2.0.24. Additionally the following
R packages must be installed: `rsyncrosim`, `tidyverse`, `raster`,  `furrr`,
`rgdal`, `logr`, `yaml`. The ST-Sim package (v3.2.25) must also be installed in
SyncroSim. The instructions to run the script assume you will be using [RStudio](https://rstudio.com/),
however, this is not a strict requirement.

### Data files

A number of data files are required that are not included on the GitHub repository due
to size constraints. These files have been compressed into a zip folder that can
be downloaded [here](https://s3.us-west-2.amazonaws.com/apexrms.com.public/Data/A236/LANDFIRE%20Update%20Example%20Data%20Files%20-%202021-01-19.zip).
Please note that this file is quite large (~5.5GB).

The zip folder also contains the expected paths of the files, and so it is
recommended to extract the zip file directly into the root of this git
repository. In other words, the `data/` folder should be present in the same
folder as this README after extraction, not inside another folder (such as
`LANDFIRE Update Data Files/`). This data archive includes all the input data
files needed to process the SC and NW Geo Areas.

## Configuration

The run can be configured by editing the `config/config.yaml` file. R Studio and
most modern text editors have syntax highlighting for YAML files that can help
with editing these files, but may not be associated with `*.yaml` files by
default. The YAML file syntax is fairly self-evident, but please see this short
[overview](https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html)
for details.

Descriptions of the configuration variables follow in the sections below.

### Overall Run Options

This section is used to decide which Geo Area and Map Zones should be processed.
The first variable, `dataFolder`, should point to the location of a folder
containing all the raw input data for a single Geo Area. The expected contents
of this folder are described in the [Input Data Structure](#InputStructure)
section, and descriptions of all input data including these Geo-Area-specific
files can be found in the [Data Dictionary](#Dictionary) section.

The second variable, `mapzonesToKeep`, is an itemized list of every Map Zone
in the Geo Area that you would like to retain in the simulation. One SyncroSim
scenario will be built for each Map Zone in this list. By default, all the Map
Zones in the NW Geo Area are listed here but are commented out, with the exception
of Map Zone 19. You can remove these comments to run the entire NW Geo Area. For
all other Geo Areas you must update the list of Map Zones to keep.

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
Out-of-Memory errors. Please refer to the [Suggested Configuration](#Suggested)
Section below for suggestions on setting `ssimJobs`.

Finally, `tileSize` can be used to decide how finely the raster maps are split
up for the spatial multiprocessing in SyncroSim. Lower values will decrease
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

## <a name="Suggested"></a>Suggested Configuration

The suggested run configuration will depend on the available compute resources.
Below is a table outlining some general recommendations and the corresponding
expected run times.

| Min. Processors | Min. Memory | SyncroSim Jobs | Tile Size | Approx. Run Time for NW Geo Area | Approx. Run Time for Map Zone 19 |
|----------------:|------------:|---------------:|----------:|---------------------------------:|---------------------------------:|
|               4 |        32GB |              2 |       150 |                         4 - 5 d  |                          4 - 8 h |
|               4 |        64GB |              4 |       150 |                         2 - 3 d  |                          2 - 4 h |
|               8 |       128GB |              8 |       250 |                           ~ 1 d  |                          1 - 2 h |
|              16 |       256GB |             16 |       250 |                       12 - 18 h  |                           ~ 45 m |
|              32 |       512GB |             32 |       250 |                        8 - 12 h  |                           ~ 30 m |

To use this table, find the last row for which you have at least the minimum
number of available processors *and* minimum amount of memory. Set the number of
SyncroSim jobs (`ssimJobs`) and tile sizes (`tileSize`) accordingly in the
config file. The remaining two columns provide ranges for the expected run times.

## <a name="Running"></a>Running the Update

Begin by opening the `LANDFIRE Update.Rproj` R project file to ensure the
correct working directory is set in RStudio. Next, open the `batchProcess.R` 
script and either run line-by-line or press the `source` button in the top right
corner of the file editor pane of RStudio.

This script is responsible for processing the raw input raster maps, including
cropping and masking down to the chosen Map Zone, converting FDist maps to
vDist, and layerizing the disturbance map for SyncroSim. The script then uses
these cleaned rasters to generate a SyncroSim library file to run the update.

Next, open the generated SyncroSim library using the SyncroSim UI. This file
will be stored in the `library/` subdirectory. Select the `NW Geo Area` project,
(or the `projectName` you chose in the configuration) and the scenario you would
like to run and press `run` in the main toolbar. Note that, depending on your
computer configuration, you may need to change the maximum number of
[multiprocessing](http://docs.syncrosim.com/how_to_guides/modelrun_multiproc.html) jobs.
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

## <a name="InputStructure"></a>Input Data Structure

To generalize the workflow for running multiple Geo Areas with minimal
reconfiguration, the scripts have strict requirements for how input files are
named and organized.

The input files for each Geo Area must be stored in its own folder, preferably
in the `data/` directory with a name that indicates which Geo Area the folder is
for. For example, the example data archive linked in the [Setup](#Setup-1) section 
contains the input files for the NW Geo Area and are accordingly are held in
`data/NW/`.

Within this directory, there must be a subdirectory named `raw/`. This is used
to separate the raw inputs from the final clean and stitched raster maps that
will be generated and stored in their respective subdirectories.

Within the `raw/` subdirectory, the raw raster maps of the Map Zones, EVC, EVH,
EVT, and FDist must be present and named `Map Zones.tif`, `EVC.tif`, `EVH.tif`,
`EVT.tif`, and `FDIST.tif` respectively.

Also required within the `raw/` subdirectory is the `nonspatial/` subdirectory,
which includes Geo-Area-specific data files that are not stored as maps. This
folder requires a CSV file of all transition rules, `Transition Table.csv`,
and a color mapping for EVT codes present in the Geo Area, `EVT Colors.csv`.

Altogether, the folder should have the following structure:

```
Geo Area Name
│
└───raw
    │   Map Zones.tif
    │   EVC.tif
    │   EVH.tif
    │   EVT.tif
    │   FDIST.tif
    │   Continuous EVC.tif
    │   Continuous EVH.tif
    │
    └───nonspatial
        │   Transition Table.csv
        │   EVT Colors.csv

```

Please see the [Data Dictionary](#Dictionary) section below for a description of
these input files and the example data files linked in the [Setup](#Setup-1)
section for the expected format of each file.

### <a name="Input_Example"></a>Importing New Data

Below is a description of the steps to prepare data for updating a Geo Area 
using the South Central Geo Area as an example.

#### Setup

To begin, create a folder for the Geo Area (`data/SC/` in this example) as well
as the necessary subfolders, `raw/` and `raw/nonspatial/`. The folder structure
will look like this:

```
SC
│
└───raw
    │
    └───nonspatial
        │   

```

#### Spatial Data

Next, we will populate the spatial data, which consists of seven raster maps of
the entire Geo Area. These maps are provided by LANDFIRE and must be downloaded,
extracted, moved into the `raw/` subfolder of the corresponding Geo Area data
folder and renamed to be recognized by the script.

For each of these seven maps, the following table lists the original file and
archive names for the SC Geo Area.

| Cleaned File Name  | Name of Archive Containing the File | Original File Name |
|:-------------------|:------------------------------------|:-------------------|
| Map Zones.tif      | sc_mapzone_tiff.zip                 | sc_mapzone.tif     |
| EVC.tif            | sc_evc2.0class1.4_tiff.zip          | sc_evc_m.tif       |
| EVH.tif            | sc_evh2.0class1.4_tiff.zip          | sc_evh_m.tif       |
| EVT.tif            | sc_evt2.0_tiff.zip                  | sc_evt20.tif       |
| FDIST.tif          | sc_fdist2020_tiff.zip               | sc_fdist.tif       |
| Continuous EVC.tif | sc_evc2.0_tiff.zip                  | sc_evc20.tif       |
| Continuous EVH.tif | sc_evh2.0_tiff.zip                  | sc_evh20.tif       |

To use this table, download the zip archives listed in the second column. Next,
extract the contents and copy the file listed in the third column into the
`raw/` subfolder created in the previous section. Finally rename the files
to the clean names listed in the first column.

For example, to prepare the raster map of Map Zones in the SC Geo Area for the
example data archive, I first downloaded and extracted `sc_mapzone_tiff.zip`.
In the uncompressed folder, I found and moved the file `sc_mapzone.tif` into
the folder `data/SC/raw/`. Finally, I renamed this raster map to
`Map Zones.tif`. After repeating this process for all seven maps, the SC data
folder should have the following structure:

```
SC
│
└───raw
    │   Map Zones.tif
    │   EVC.tif
    │   EVH.tif
    │   EVT.tif
    │   FDIST.tif
    │   Continuous EVC.tif
    │   Continuous EVH.tif
    │
    └───nonspatial
        │

```

#### Non-Spatial Data

Next, we will prepare the two non-spatial data files needed for each Geo Are.
Both of these files are provided by LANDFIRE as tables within a Microsoft Access
database file. These Access database tables must be exported, converted to Comma
Separated Value (CSV) format, moved into the `raw/nonspatial/` subfolder of the
corresponding Geo Area data folder and renamed to be recognized for the script.

Both tables needed for the SC Geo Area were found within the
`SC_GeoArea_VegTransitions_Update_for_Remap_KCH_complete_KCH_2020_12_18.accdb`
database file. The following table connects the non-spatial data file names
with their original table names in the Access database.

| Cleaned File Name    | Original Table Name within Database |
|:---------------------|:------------------------------------|
| Transition Table.csv | vegtransf_rv02i_d                   |
| EVT Colors.csv       | sc_evt200                           |

To use this table, open the Access database file using Microsoft Access and
locate the tables listed in the second column. Right-click each table and
select `Export` > `Excel` to export the tables to Excel (*.xlsx) format. Open
the resulting Excel spreadsheets in Microsoft Excel and use the `File` > 
`Save As` dialog to convert the file to a CSV file. Finally, move the files to
the `raw/nonspatial/` subfolder and rename the files to the clean file names
listed in the first column.

For example, to prepare the transition table for the SC Geo Area, I first
downloaded the Access database file. I located the `vegtransf_rv02i_d` table
within this database, exported it into an Excel file using Access, and then
used Excel to convert the file into a CSV. I moved this CSV file into the
folder `data/SC/raw/nonspatial/` and renamed it to `Transition Table.csv`.
After repeating these steps for both nonspatial datasets, you should have
all the necessary data to process the entire Geo Area, with your folder/file 
structure shown below.

```
SC
│
└───raw
    │   Map Zones.tif
    │   EVC.tif
    │   EVH.tif
    │   EVT.tif
    │   FDIST.tif
    │   Continuous EVC.tif
    │   Continuous EVH.tif
    │
    └───nonspatial
        │  Transition Table.csv
        |  EVT Colors.csv

```

#### Updating the Config File

Once the data files for a new Geo Area have been prepared, it is important to
update the configuration file `config/config.yaml` to reflect these changes.
Please refer to the [Configuration](#Configuration-1) section for details, but
be sure to update the path to the new data folder (`dataFolder`), the list of
map zones to generate scenarios for (`mapzonesToKeep`), and the SyncroSim
library and project names (`libraryName` and `projectName`, respectively). The
crop extent used for testing (`cropExtentPath`) must also be updated to use
testing mode with a new Geo Area.

## <a name="Dictionary"></a>Data Dictionary

Below is a description of all data files used in the update. This is includes
both spatial and non-spatial inputs. A number of data files are common to all
Geo Areas and are described in the [Shared Data Files](#Shared) subsection below.
The remaining input data files are described in the [Geo-Area-Specific Data Files](#Specific)
subsection. Descriptions of the columns within each table can also be found
within these subsections.

### <a name="Shared"></a>Shared Data Files

These common files are stored in the `data/Shared/` folder and are used for all
Geo Areas.

| File Name                                    | Description                                                                                                              |
|:---------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------|
| [All Combinations.csv](#Dict_Combinations)   | This table provides every valid combination of EVC and EVH. This is used to check for invalid state classes.             |
| [Disturbance Crosswalk.csv](#Dict_Cross)     | This crosswalk is used to convert between Fuel Disturbance (FDist) and Vegetation Disturbance (VDist) codes.             |
| [EVC LUT.csv](#Dict_EVC)                     | This look-up table is used connect to Existing Vegetation Cover (EVC) codes to human-readable names.                     |
| [EVH LUT.csv](#Dict_EVH)                     | This look-up table is used connect to Existing Vegetation Height (EVH) codes to human-readable names.                    |
| [VDIST LUT.csv](#Dict_VDIST)                 | This look-up table is used connect to Vegetation Disturbace (VDist) codes to human-readable names.                       |
| [EVC Colors.csv](#Dict_EVCcol)               | This table is used to assign colors to Existing Vegetation Cover (EVC) codes. These are used when rendering raster maps. |
| [Default SyncroSim Charts.csv](#Dict_Charts) | This table is used to build the default SyncroSim charts for visualizing the run results.                                |
| [Default SyncroSim Maps.csv](#Dict_Charts)   | This table is used to build the default SyncroSim maps for visualizing the run results.                                  |

#### <a name="Dict_Combinations"></a> All Combinations.csv

| Column | Description                                                                   |
|:-------|:------------------------------------------------------------------------------|
| EVC    | An Existing Vegetation Cover (EVC) code.                                       |
| EVH    | An Existing Vegetation Cover (EVH) code that can be paired with the given EVC. |

#### <a name="Dict_Cross"></a> Disturbance Crosswalk.csv

| Column     | Description                               |
|:-----------|:------------------------------------------|
| VDIST      | A Vegetation Disturbance (VDist) code.    |
| FDIST      | A matching Fuel Disturbance (FDist) code. |
| d_type     | The type of disturbance.                  |
| d_severity | The severity of the disturbance.          |
| d_time     | The time since the disturbance.           |

#### <a name="Dict_EVC"></a> EVC LUT.csv

| Column       | Description                                                                                       |
|:-------------|:--------------------------------------------------------------------------------------------------|
| VALUE        | An Existing Vegetation Cover (EVC) code.                                                          |
| CLASSNAMES   | A matching name for the cover code.                                                               |
| EVT_LIFEFORM | A grouping variable describing the lifeform type.                                                 |
| CONTINUOUS   | A matching continuous Existing Vegetation Cover (EVC) code to use during Geo Area reconstruction. |

#### <a name="Dict_EVH"></a> EVH LUT.csv

| Column       | Description                                                                                        |
|:-------------|:---------------------------------------------------------------------------------------------------|
| VALUE        | An Existing Vegetation Cover (EVH) code.                                                           |
| CLASSNAMES   | A matching name for the cover code.                                                                |
| LIFEFORM     | A grouping variable describing the lifeform type.                                                  |
| HC_ID        | Not used.                                                                                          |
| CONTINUOUS   | A matching continuous Existing Vegetation Height (EVC) code to use during Geo Area reconstruction. |


#### <a name="Dict_VDIST"></a> VDIST LUT.csv

| Column     | Description                                                               |
|:-----------|:--------------------------------------------------------------------------|
| value      | A Vegetation Disturbance (VDist) code.                                    |
| d_type     | The type of disturbance.                                                  |
| d_severity | The severity of the disturbance.                                          |
| d_time     | The time since the disturbance.                                           |
| R          | The Red component of the disturbance color as an integer from 0 to 255.   |
| G          | The Green component of the disturbance color as an integer from 0 to 255. |
| B          | The Blue component of the disturbance color as an integer from 0 to 255.  |

#### <a name="Dict_EVCcol"></a> EVC Colors.csv

| Column     | Description                                                               |
|:-----------|:--------------------------------------------------------------------------|
| value      | An Existing Vegetation Cover (EVC) code.                                  |
| R          | The Red component of the state class color as an integer from 0 to 255.   |
| G          | The Green component of the state class color as an integer from 0 to 255. |
| B          | The Blue component of the state class color as an integer from 0 to 255.  |

#### <a name="Dict_Charts"></a> Default SyncroSim Charts and Maps

These two CSV files are generated and exported from SyncroSim and are used to
construct the default charts and maps. It is not recommended to edit these
tables by hand.

### <a name="Specific"></a>Geo-Area-Specific Data Files

These files are specific to the Geo Area being processed. As described in the
[Input Data Structure](#InputStructure) section, these files are suggested to be
stored in a folder within the `data/` folder that indicates the Geo Area name.
This section also describes the expected organization of these files within this
Geo-Area-specific folder.

| File Name                                | Description                                                                                                                                        |
|:-----------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------|
| [Transition Table.csv](#Dict_Transition) | This table defines how every combination of Existing Vegetation Cover (EVC), Height (EVH), and Type (EVT) responds to a given disturbance (VDist). |
| [EVT Colors.csv](#Dict_EVT)              | This table is used to assign colors to Existing Vegetation Type (EVT) codes. These are used when rendering raster maps.                            |
| Map Zones.tif                            | This is the raster map of Map Zones found within the Geo Area.                                                                                     |
| EVC.tif                                  | This is the raster map of Existing Vegetation Covers (EVC) for the entire Geo Area using the EVC class codes.                                      |
| EVH.tif                                  | This is the raster map of Existing Vegetation Heights (EVH) for the entire Geo Area using EVH class codes.                                         |
| EVT.tif                                  | This is the raster map of Existing Vegetation Types (EVT) for the entire Geo Area.                                                                 |
| FDIST.tif                                | This is the raster map of Fuel Disturbances (FDist) for the entire Geo Area.                                                                       |
| Continuous EVC.tif                       | This is the raster map of Existing Vegetation Covers (EVC) for the entire Geo Area using the continuous EVC codes.                                 |
| Continuous EVH.tif                       | This is the raster map of Existing Vegetation Heights (EVH) for the entire Geo Area using the continuous EVH codes.                                |

#### <a name="Dict_Transition"></a> Transition Table.csv

| Column     | Description                                                              |
|:-----------|:-------------------------------------------------------------------------|
| MZ         | The Map Zone to apply the transition rule.                               |
| VDIST      | The Vegetation Disturbance (VDist) code that triggers the transition.    |
| DIST_CATID | Not used.                                                                |
| EVT7B      | The Existing Vegetation Type (EVT) code prior to the disturbance.        |
| EVT7B_Name | The name of the Existing Vegetation Type (EVT) prior to the disturbance. |
| EVHB       | The Existing Vegetation Cover (EVC) code prior to the disturbance.       |
| EVCB       | The Existing Vegetation Height (EVH) code prior to the disturbance.      |
| EVT7R      | Not used.                                                                |
| EVT7R_Name | Not used.                                                                |
| EVHR       | The Existing Vegetation Cover (EVC) code after the disturbance.          |
| EVCR       | The Existing Vegetation Height (EVH) code after the disturbance.         |

#### <a name="Dict_EVT"></a> EVT Colors.csv

| Column     | Description                                                                  |
|:-----------|:-----------------------------------------------------------------------------|
| VALUE      | An Existing Vegetation Type (EVT) code.                                      |
| COUNT      | Not used.                                                                    |
| VALUE_1    | Not used.                                                                    |
| EVT_NAME   | Not used.                                                                    |
| LFRDB      | Not used.                                                                    |
| EVT_FUEL   | Not used.                                                                    |
| EVT_FUEL_N | Not used.                                                                    |
| EVT_LF     | Not used.                                                                    |
| EVT_PHYS   | Not used.                                                                    |
| EVT_GP     | Not used.                                                                    |
| EVT_GP_N   | Not used.                                                                    |
| SAF_SRM    | Not used.                                                                    |
| EVT_ORDER  | Not used.                                                                    |
| EVT_CLASS  | Not used.                                                                    |
| EVT_SBCLS  | Not used.                                                                    |
| R          | The Red component of the primary stratum color as an integer from 0 to 255.  |
| G          | The Green component of the primary stratum color as an integer from 0 to 255.|
| B          | The Blue component of the primary stratum color as an integer from 0 to 255. |
| RED        | Not used.                                                                    |
| GREEN      | Not used.                                                                    |
| BLUE       | Not used.                                                                    |
