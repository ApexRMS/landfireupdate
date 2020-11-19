### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### November 2020
### Function to consolidate scenarios written to separate libraries by
### `scripts/buildSsimLibraries.R` to a single file

consolidateSsimLibraries <- function(libraryName, projectName, parallelLibraryNames) {
  # Create library with a project ("Definitions") and a scenario ("Test")
  consolidatedLibrary <- ssimLibrary(libraryName, overwrite = TRUE)
  consolidatedProject <- rsyncrosim::project(consolidatedLibrary, projectName, overwrite = TRUE)
  
  # Set owner
  owner(consolidatedLibrary) <- ssimOwner
  owner(consolidatedProject) <- ssimOwner
  
  # Set descriptions
  description(consolidatedLibrary)  <- libraryDescription
  description(consolidatedProject)  <- projectDescription

  walk(
    parallelLibraryNames,
    function(sourceLibrary) {
      ssimCommandArgs <- 
        list(copy = NULL,
             scenario = NULL,
             slib = paste0(sourceLibrary, ".ssim"),
             tlib = paste0(libraryName, ".ssim"),
             sid = 1)
      rsyncrosim::command(ssimCommandArgs)
    }
  )
}
