### LANDFIRE Project
### APEX RMS - Valentin Lucet and Shreeram Senthivasan
### November 2020
### Function to generate SyncroSim library, project, and scenarios for each Map
### Zone using the cleaned rasters genereated by `scripts/processSpatialData.R`
### and `scripts/layerizeDisturbance.R`

initializeSsimLibrary <- function(libraryName, projectName) {
  # Prepare input data ---------------------------------------------------

  # Load the lookup table that will be used to color state classes by their EVC code
  # This table defines colors for cover type in 1% bins, but the data is in 10% bins
  # The filter and mutate steps below convert the 1% binned EVC codes to 10% codes
  evcColors <- read_csv(evcColorsPath) %>%
    transmute(
      evcCode = VALUE,
      Color = paste("255", R, G, B, sep = ",")
    ) %>%
    # Only the codes above 100 differ between the two EVC code systems
    # Of these differing codes, we keep every 10th (eg. 10%, 20%, etc )
    filter(evcCode < 100 | evcCode %% 10 == 0) %>%
    # The 1% resolution code stores lifeform (tree, shrub, etc) in the 100th's digit,
    # The 10% stores this in the 10th's digit, so we divide by 10
    # Finally we add 90 to shift the correct color to the the correct lifeform
    mutate(
      evcCode = if_else(evcCode <= 100,
                        true = evcCode,
                        false = evcCode / 10 + 90),
      evcCode = as.character(evcCode)
    )

  ## Generate a table of all unique transitions

  # Should be unique for every VDIST, PrimaryStratum, EvT, SourceStateClass
  transitionTable <- read_xlsx(transitionTablePath, col_types = "text") %>%
    # Select and rename variables of importance
    dplyr::select(MZ, VDIST, EVT7B, StratumIDSource = EVT7B_Name,
           EVCB, EVHB, EVCR, EVHR) %>%
    # Turn all factors into strings
    mutate_if(is.factor, as.character) %>%
    # Change the naming convention of MapZones e.g. from "1" to "MZ01"
    mutate(SecondaryStratumID = paste0("MZ", str_pad(MZ, 2, "left", "0"))) %>%
    # Keep only unique rows
    unique()

  ## Generate look-up tables for EVC and EVH codes and names

  EVClookup <- read_xlsx(evcTablePath, col_types = "text") %>%
    # Turn factors into characters
    mutate_if(is.factor, as.character) %>%
    # Make unique names for when class names are repeated
    mutate(CLASSNAMES = coalesce(CLASSNAMES, str_c(EVT_LIFEFORM, "_", VALUE))) %>%
    # Select relevant columns and rename with stsim relevant column names
    dplyr::select(VALUE, CLASSNAMES) %>%
    rename(EVC_ID = VALUE, StateLabelXID = CLASSNAMES) %>%
    # Shorten EVC names for cleaner SyncroSim UI
    mutate(
      StateLabelXDescription = StateLabelXID,
      StateLabelXID = str_replace(StateLabelXID, " and <=? ", "-"),
      StateLabelXID = str_replace(StateLabelXID, "Tree Cover >=",  "Tr"),
      StateLabelXID = str_replace(StateLabelXID, "Tree Cover <",  "Tr <"),
      StateLabelXID = str_replace(StateLabelXID, "Shrub Cover >=", "Sh"),
      StateLabelXID = str_replace(StateLabelXID, "Herb Cover >=",  "Hb")
    )

  EVHlookup <- read_xlsx(evhTablePath, col_types = "text") %>%
    mutate_if(is.factor, as.character) %>%
    mutate(CLASSNAMES = coalesce(CLASSNAMES, str_c(LIFEFORM, "_", VALUE))) %>%
    dplyr::select(VALUE, CLASSNAMES) %>%
    rename(EVH_ID = VALUE, StateLabelYID = CLASSNAMES) %>%
    # Shorten EVH names for cleaner SyncroSim UI
    mutate(
      StateLabelYDescription = StateLabelYID,
      StateLabelYID = str_replace(StateLabelYID, " to ", "-"),
      StateLabelYID = str_replace(StateLabelYID, " meters?", "m"),
      StateLabelYID = str_replace(StateLabelYID, "Forest Height", "Fr"),
      StateLabelYID = str_replace(StateLabelYID, "Shrub Height",  "Sh"),
      StateLabelYID = str_replace(StateLabelYID, "Herb Height",   "Hb"),
      StateLabelYID = str_replace(StateLabelYID, " 0-",   " < "),
    )

  # Add EVC and EVH names to transition table

  transitionTable <- transitionTable %>%

    # First join EVC B and R, renaming appropriately
    left_join(EVClookup, by = c("EVCB" = "EVC_ID")) %>%
    rename(EVCB_Name = StateLabelXID) %>%
    left_join(EVClookup, by = c("EVCR" = "EVC_ID")) %>%
    rename(EVCR_Name = StateLabelXID) %>%
    dplyr::select(-StateLabelXDescription.x, -StateLabelXDescription.y) %>%

    # Similarly for EVH
    left_join(EVHlookup, by = c("EVHB" = "EVH_ID")) %>%
    rename(EVHB_Name = StateLabelYID) %>%
    left_join(EVHlookup, by = c("EVHR" = "EVH_ID")) %>%
    rename(EVHR_Name = StateLabelYID) %>%
    dplyr::select(-StateLabelYDescription.x, -StateLabelYDescription.y) %>%

    # Create the State Class names from EVC : EVH combinations
    mutate(StateClassIDSource = paste0(EVCB_Name, " : ", EVHB_Name),
           StateClassIDDest = paste0(EVCR_Name, " : ", EVHR_Name)) %>%

    # Add the propability column with all set to 1
    mutate(Probability = 1)

  # Build the SyncroSim Library ---------------------------------------------

  # Create library with a project ("Definitions") and a scenario ("Test")
  dir.create("library/", showWarnings = FALSE)
  ssimSession <- session(ssimDir)
  mylibrary <- ssimLibrary(libraryName, session = ssimSession, overwrite = TRUE)
  myproject <- rsyncrosim::project(mylibrary, projectName, overwrite = TRUE)
  myscenario <- scenario(myproject, subScenarioName, overwrite = TRUE)

  # Set owner
  owner(mylibrary) <- ssimOwner
  owner(myproject) <- ssimOwner
  owner(myscenario) <- ssimOwner

  # Set descriptions
  description(mylibrary)  <- libraryDescription
  description(myproject)  <- projectDescription
  description(myscenario)  <- subScenarioDescription

  ## +Terminology -----------------------------------------------------------

  term <- data.frame(
    AmountLabel = "Area",
    AmountUnits = "Acres",
    StateLabelX = "EVC",
    StateLabelY = "EVH",
    PrimaryStratumLabel = "EVT",
    SecondaryStratumLabel = "Map Zone",
    TimestepUnits = "Timestep"
  )

  saveDatasheet(ssimObject = myproject, data = term,
                name = "stsim_Terminology")


  ## +Strata ----------------------------------------------------------------

  # EVT is the primary stratum, MZ is the secondary stratum
  # No tertiary stratum
  # Important to take the unique values every time
  # Filter this dataframe by EVT values that are actually present in the input


  primary <- data.frame(ID = transitionTable$EVT7B,
                        Name = transitionTable$StratumIDSource) %>%
    unique()

  # Extract colors from the evt200 sheet
  # TODO: An issue here where some IDs do not have colors and some colors do not
  # have matching IDs.

  primaryWithColors <- read_xlsx(evtColorTablePath, col_types = "text") %>%
    # Select relevant columns
    dplyr::select(VALUE, R, G, B) %>%
    # Take unique and rename for later joining
    unique() %>% rename(ID = VALUE) %>%
    # Create the color using the SyncroSim pattern of T, R, G, B
    mutate(Color = paste("255", R, G, B, sep = ",")) %>%
    # Join and select relevant columns
    right_join(primary, by = "ID") %>%
    dplyr::select(ID, Color, Name) %>%
    as.data.frame()

  # Save the datasheet
  saveDatasheet(myproject, primaryWithColors, "Stratum")

  # Repeat for secondary stratum (MapZone), with no colors

  secondary <- data.frame(ID = transitionTable$MZ,
                          Name = transitionTable$SecondaryStratumID) %>% unique()
  saveDatasheet(myproject, secondary, "SecondaryStratum")

  ## +State Classes --------------------------------------------------------

  # Copy in X and Y state names from the EVC and EVH lookups, respectively

  state_x <- data.frame(
    Name = EVClookup$StateLabelXID,
    Description = EVClookup$StateLabelXDescription) %>%
    unique()

  saveDatasheet(myproject, state_x, "stsim_StateLabelX")

  state_y <- data.frame(
    Name = EVHlookup$StateLabelYID,
    Description = EVHlookup$StateLabelYDescription) %>%
    unique()

  saveDatasheet(myproject, state_y, "stsim_StateLabelY")

  # Generate unique State IDS based on the combination of X and Y state
  # To do this, we "paste" the X and Y state IDs together by multiplying
  # EVC by 1000 and adding it to EVH

  stateIDs <- c((as.numeric(transitionTable$EVCB)*1000 + as.numeric(transitionTable$EVHB)),
                (as.numeric(transitionTable$EVCR)*1000 + as.numeric(transitionTable$EVHR)))

  # Build the datasheet
  stateClasses <- data.frame(
    ID = stateIDs,
    Name = c(transitionTable$StateClassIDSource, transitionTable$StateClassIDDest),
    StateLabelXID = c(transitionTable$EVCB_Name, transitionTable$EVCR_Name),
    evcCode = c(transitionTable$EVCB, transitionTable$EVCR),
    StateLabelYID = c(transitionTable$EVHB_Name, transitionTable$EVHR_Name)) %>%
    left_join(evcColors, by = "evcCode") %>% # Use EVC to decide state color
    dplyr::select(-evcCode) %>%                     # Remove code used to add colors
    unique()

  saveDatasheet(myproject, stateClasses, "stsim_StateClass")

  ## +Transition Types and Groups ------------------------------------------------

  # We gather disturbance types from the VDIST table
  vdistLookup <-  read_xlsx(vdistTablePath, col_types = "text") %>%
    # Select only what we need, then rename
    dplyr::select(value, d_type, d_severity, d_time, R, G, B) %>%
    rename(ID = value,  TransitionGroupID = d_type) %>%
    # Turn all factors into charactors
    mutate_if(is.factor, as.character) %>%
    # Filter out the NO Disturbance category
    filter(ID != 0) %>%
    # Create unique transition/disturbance name, and format color
    # The format of the name is : Group, Severity, Frequency
    mutate(Name = paste(TransitionGroupID, d_severity, d_time, sep = " - ")) %>%
    mutate(Color = paste("255", R, G, B, sep = ","))

  # Select the relevant columns, and filter by disturbances that are actually
  # present in the input raster
  transitionTypes <- vdistLookup %>%
    dplyr::select(ID, Name, Color) %>%
    unique() %>%
    as.data.frame()

  saveDatasheet(myproject, transitionTypes, "stsim_TransitionType")

  ## Transition Groups
  # For groups, we append the disturbance class to the existing datasheet

  transitionGroups <- datasheet(myproject, "stsim_TransitionGroup") %>%
    mutate_if(is.factor, as.character) %>%
    bind_rows(vdistLookup %>%
                dplyr::select(Name = TransitionGroupID) %>%
                unique()) %>%
    as.data.frame()

  saveDatasheet(myproject, transitionGroups, "stsim_TransitionGroup")

  ## Transition Types by groups

  typesByGroup <- vdistLookup %>%
    dplyr::select(ID, Name, TransitionGroupID) %>%
    unique() %>%
    dplyr::select(-ID) %>%
    rename(TransitionTypeID = Name) %>%
    as.data.frame()

  saveDatasheet(myproject, typesByGroup, "stsim_TransitionTypeGroup")

  ## Transition simulation groups
  # The same than groups, used for vizualization

  simulationGroups <- data.frame(
    TransitionGroupID = unique(vdistLookup$TransitionGroupID))

  saveDatasheet(myproject, simulationGroups, "stsim_TransitionSimulationGroup")

  ## +Transitions  ---------------------------------------------------------------

  ## Deterministic

  # Generate locations for unique state classes to be used in the SyncroSim
  # Transition Pathways Diagrams

  locations <-
    stateClasses %>%
      mutate(
        letter = case_when(
          str_detect(StateLabelXID, "10-20")   ~ "A",
          str_detect(StateLabelXID, "20-30")   ~ "B",
          str_detect(StateLabelXID, "30-40")   ~ "C",
          str_detect(StateLabelXID, "40-50")   ~ "D",
          str_detect(StateLabelXID, "50-60")   ~ "E",
          str_detect(StateLabelXID, "60-70")   ~ "F",
          str_detect(StateLabelXID, "70-80")   ~ "G",
          str_detect(StateLabelXID, "80-90")   ~ "H",
          str_detect(StateLabelXID, "90-100")  ~ "I",
          str_detect(StateLabelXID, "< 10")    ~ "J", # J is reserved for uncommon cover labels
          str_detect(StateLabelXID, "Sparse")  ~ "J", # J is reserved for uncommon cover labels
          TRUE                                 ~  NA_character_),
        letter = suppressWarnings(replace(letter, is.na(letter), str_to_upper(letters))),
        number = case_when(
          str_detect(StateLabelYID, "Fr > ")   ~  1,
          str_detect(StateLabelYID, "Fr 25")   ~  2,
          str_detect(StateLabelYID, "Fr 10")   ~  3,
          str_detect(StateLabelYID, "Fr 5-")   ~  4,
          str_detect(StateLabelYID, "Fr < ")   ~  5,
          str_detect(StateLabelYID, "Sh > ")   ~  6,
          str_detect(StateLabelYID, "Sh 1.0")  ~  7,
          str_detect(StateLabelYID, "Sh 0.5")  ~  8,
          str_detect(StateLabelYID, "Sh < ")   ~  9,
          str_detect(StateLabelYID, "Hb > ")   ~ 10,
          str_detect(StateLabelYID, "Hb 0.5")  ~ 11,
          str_detect(StateLabelYID, "Hb < ")   ~ 12,
          TRUE                                 ~ 13),
        # Deal with mixed forms
          mixedLifeForm = case_when(
            str_detect(StateLabelXID, "Tr") & !str_detect(StateLabelYID, "Fr") ~ T,
            str_detect(StateLabelXID, "Sh") & !str_detect(StateLabelYID, "Sh") ~ T,
            str_detect(StateLabelXID, "Hb") & !str_detect(StateLabelYID, "Hb") ~ T,
            T                                                                  ~ F),
          number = if_else(mixedLifeForm, number+14, number),
        # Done dealing with mixed lifeforms
        Location = str_c(letter, number)) %>%
      dplyr::select(Name, Location)


  # Join the locations back into the state class table and clean up the datasheet
  deterministicTransitions <- stateClasses %>%
    # Make sure to expand the grid to all combinations of MZ present
    expand_grid(StratumIDSource = primary$Name) %>%
    # Join with the location info
    left_join(locations, by = "Name") %>%
    # Cleanup
    dplyr::select(Name, Location, StratumIDSource) %>%
    rename(StateClassIDSource = Name) %>%
    unique() %>%
    as.data.frame()

  saveDatasheet(myscenario, deterministicTransitions,
                "stsim_DeterministicTransition")

  ## Probabilistic
  probabilisticTransitions <- transitionTable %>%
    # Join the IDS
    left_join(transitionTypes, by = c("VDIST" = "ID")) %>%
    # Rename and select what we need
    rename(TransitionTypeID = Name) %>%
    dplyr::select(StratumIDSource, SecondaryStratumID,
                  StateClassIDSource, StateClassIDDest,
                  TransitionTypeID, Probability) %>%
    as.data.frame()

  saveDatasheet(myscenario, probabilisticTransitions, "stsim_Transition")

  ## +Run Control --------------------------------------------------------------

  runControl <- data.frame(
    MinimumIteration = minimumIteration,
    MaximumIteration = maximumIteration,
    MinimumTimestep =  minimumTimestep,
    MaximumTimestep =  maximumTimestep,
    IsSpatial = TRUE
  )

  saveDatasheet(myscenario, runControl, "stsim_RunControl")

  ## +Output Options -----------------------------------------------------------

  outputOptionsSummary <-
    data.frame(SummaryOutputSC = TRUE, SummaryOutputSCTimesteps = 1,
               SummaryOutputTR = TRUE, SummaryOutputTRTimesteps = 1)
  outputOptionsSpatial <-
    data.frame(RasterOutputSC = TRUE, RasterOutputSCTimesteps = 1,
               RasterOutputST = TRUE, RasterOutputSTTimesteps = 1,
               RasterOutputTR = TRUE, RasterOutputTRTimesteps = 1)

  saveDatasheet(myscenario, outputOptionsSummary, "stsim_OutputOptions")
  saveDatasheet(myscenario, outputOptionsSpatial, "stsim_OutputOptionsSpatial")

  ## +Multiprocessing -----------------------------------------------------------

  multiprocessing <-
    data.frame(EnableMultiprocessing = TRUE,
               MaximumJobs = ssimJobs,
               EnableMultiScenario = FALSE)

  saveDatasheet(myscenario, multiprocessing, "core_Multiprocessing")
}

buildSsimScenarios <- function(runTag, scenarioName, scenarioDescription, libraryName, projectName) {
  # Generate run-specific file paths ---------------------------------------
  
  # Directory to store cleaned rasters
  # Note that the working directory is prepended since SyncroSim needs absolute paths
  cleanRasterDirectory <- paste0(getwd(), "/data/clean/", runTag, "/")
  
  # Directory and prefix for FDIST binary rasters (spatial multipliers)
  transitionMultiplierDirectory <- paste0(cleanRasterDirectory, "transitionMultipliers/")
  
  # Clean Raster Paths
  stateClassRasterPath <- paste0(cleanRasterDirectory, "StateClass.tif")
  primaryStratumRasterPath <- paste0(cleanRasterDirectory, "EVT.tif")
  secondaryStratumRasterPath <- paste0(cleanRasterDirectory, "MapZone.tif")

  tilingRasterPath <- paste0(cleanRasterDirectory, "Tiling.tif")

  # Build Scenario ------------------------------------------------------------
  ssimSession <- session(ssimDir)
  mylibrary <- ssimLibrary(libraryName, session = ssimSession)
  myproject <- rsyncrosim::project(mylibrary, projectName)
  myscenario <- scenario(myproject, scenarioName, overwrite =T)
  description(myscenario) <- scenarioDescription
  
  ## +Common Dependency --------------------------------------------------------
  
  # Add the Sub Scenario as a dependency to import common model info
  dependency(myscenario, dependency = subScenarioName)
  
  ## +Transition multipliers ---------------------------------------------------
  
  # Collect the names and cretae path files
  multiplierGroupNames <- 
    transitionMultiplierDirectory %>%
    list.files %>%
    str_sub(end = -5) %>%
    str_c(" [Type]")
  
  multiplierFileNames <- 
    transitionMultiplierDirectory %>%
    list.files(full.names = T)
  
  # Compose and save the data frame
  if(length(multiplierFileNames) > 0) {
  spatialMultiplier <- data.frame(
    TransitionGroupID = multiplierGroupNames,
    MultiplierFileName = multiplierFileNames)
  
  saveDatasheet(myscenario, spatialMultiplier,
                "stsim_TransitionSpatialMultiplier")
  } else
    warning(paste0("There were no disturbances found in ", runTag,
                   ". This is not necessarily an error, please check the raw data."))
  
  ## +Initial conditions --------------------------------------------------------
  
  initialConditionsSpatial <- data.frame(
    StateClassFileName = stateClassRasterPath,
    StratumFileName = primaryStratumRasterPath,
    SecondaryStratumFileName = secondaryStratumRasterPath)
  
  saveDatasheet(myscenario, initialConditionsSpatial,
                "stsim_InitialConditionsSpatial")
  
  ## +Spatial multiprocessing ---------------------------------------------------
  
  spatialMultiprocessing <- datasheet(myscenario, "corestime_Multiprocessing")
  spatialMultiprocessing <- add_row(spatialMultiprocessing, MaskFileName = tilingRasterPath)
  
  saveDatasheet(myscenario, spatialMultiprocessing, "corestime_Multiprocessing")
}
