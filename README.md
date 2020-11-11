# Landfire Update

Code to prepare data for Landfire Update using ST-Sim. This project consists of
a collection of scripts to (A) process and clean raw spatial data and (B) use
these data to generate a SyncroSim library to run the Landfire Update itself.

## Setup

### Dependencies

These scripts require working installations of R and SyncroSim, and were
developed on R version v4.0.3 and SyncroSim v2.0.22. Additionally the following
R packages must be installed: rsyncrosim, tidyverse, raster,  furrr, readxl,
RODBC, rgdal. The ST-Sim package must also be installed in SyncroSim.

### Data files

A number of data files are required that are not included on the GitHub repo due
to size constraints. They, along with their URL's and suggested local locations
are listed in the table below:

**Add Table**

## Running the Update

### Configuration

It is recommended to configure the script to set run options, including which
map zones to process, how many cores to use, etc., as well as to ensure that
input file paths are correctly set.
