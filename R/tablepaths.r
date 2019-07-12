# output station table path
get_output_station_table = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady",
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type %in% c("Unsteady", "Unsteady+Sediment"))
    switch(RAS.version,
      "5.0.3" = file.path("Results", "Unsteady", "Geometry Info",
        "Cross Section Only"),
      "5.0.4" = file.path("Results", "Unsteady", "Geometry Info",
        "Cross Section Only"),
      "5.0.5" = file.path("Results", "Unsteady", "Geometry Info",
        "Cross Section Only"),
      "5.0.6" = file.path("Results", "Unsteady", "Geometry Info",
        "Cross Section Only")
    )
  else if (run.type == "Steady")
    stop("not implemented!")
  else
    switch(RAS.version,
      "5.0.3" = file.path("Results", "Sediment", "Geometry Info",
      "Cross Section Only"),
      "5.0.4" = file.path("Results", "Sediment", "Geometry Info",
      "Cross Section Only"),
      "5.0.5" = file.path("Results", "Sediment", "Geometry Info",
      "Cross Section Only"),
      "5.0.6" = file.path("Results", "Sediment", "Geometry Info",
      "Cross Section Only")
    )
}

# station table path
get_station_table = function(RAS.version) {
  if (is.null(RAS.version))
    RAS.version = options()[["RAStestR.RASversion"]]
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "River Stations"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

# node name table path
get_nodename_table = function(RAS.version) {
  if (is.null(RAS.version))
    RAS.version = options()[["RAStestR.RASversion"]]
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Node Names"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

# node description table path
get_nodedesc_table = function(RAS.version) {
  if (is.null(RAS.version))
    RAS.version = options()[["RAStestR.RASversion"]]
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Node Descriptions"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

# river table path
get_river_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "River Names"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

# reach table path
get_reach_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Reach Names"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}

# reach lengths table path
get_lengths_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Lengths"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}
# grain class table path
get_grain_class_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Event Conditions", "Sediment", "Grain Class Names"),
    "5.0.4" = file.path("Sediment", "Grain Class Data", "Grain Class Names"),
    "5.0.5" = file.path("Sediment", "Grain Class Data", "Grain Class Names")
  )
}
# bank station table path
get_bank_stations_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Bank Stations"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}
# levees table path
get_levees_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "Cross Sections", "Levees"),
    "5.0.4" = file.path("Geometry", "Cross Sections", "Attributes"),
    "5.0.5" = file.path("Geometry", "Cross Sections", "Attributes")
  )
}
# output interval table path
get_timestep_table = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady",
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
  else if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Base Output", "Unsteady Time Series", "Time Date Stamp")
  else if (run.type == "Steady")
    file.path("Results", "Steady", "Output", "Output Blocks",
      "Base Output", "Steady Profiles", "Profile Names")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Time Date Stamp")
  }

# parent path of output data
get_output_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady",
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Base Output", "Unsteady Time Series", "Cross Sections")
  else if (run.type == "Steady")
    file.path("Results", "Steady", "Output", "Output Blocks",
      "Base Output", "Steady Profiles", "Cross Sections")
  else
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  }

# parent path of sediment output data
get_sediment_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady",
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else if (run.type == "QuasiUnsteady")
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment", "Sediment Time Series", "Cross Sections")
  else
    stop(sprintf('No sediment data available for run type "%s"', run.type))
  }

# parent path of cross section output data
get_xsection_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady",
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady+Sediment")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
  else if (run.type == "QuasiUnsteady")
    file.path("Results", "Sediment", "Output Blocks",
      "Sediment SE", "Sediment Time Series", "Cross Section SE")
  else
    stop(sprintf('No sediment data available for run type "%s"', run.type))
  }

# parent path of cross section output data in geometry
get_xsection_block_geometry = function(RAS.version) {
  file.path("Geometry", "Cross Sections")
}



# parent path of 2D flow area output data
get_2darea_block = function(run.type, RAS.version) {
  run.type = match.arg(run.type, c("Steady", "Unsteady",
    "Unsteady+Sediment", "QuasiUnsteady"))
  if (run.type == "Unsteady")
    file.path("Results", "Unsteady", "Output", "Output Blocks",
      "Base Output", "Unsteady Time Series", "2D Flow Areas")
  else
    stop(sprintf('No 2D data available for run type "%s"', run.type))
  }

# Plan Information Path
get_plan_info_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Plan Data", "Plan Information"),
    "5.0.4" = file.path("Plan Data", "Plan Information"),
    "5.0.5" = file.path("Plan Data", "Plan Information")
  )
}

# 2D flow area names
get_2d_flow_area_table = function(RAS.version) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "2D Flow Areas", "Names"),
    "5.0.4" = file.path("Geometry", "2D Flow Areas", "Attributes"),
    "5.0.5" = file.path("Geometry", "2D Flow Areas", "Attributes")
  )
}

# path to results metadata
get_results_meta_table = function(run.type, RAS.version) {
  if (run.type %in% c("Unsteady", "Unsteady+Sediment")) {
    switch(RAS.version,
      "5.0.3" = file.path("Results", "Unsteady", "Summary"),
      "5.0.4" = file.path("Results", "Unsteady", "Summary"),
      "5.0.5" = file.path("Results", "Unsteady", "Summary")
    )
  } else if (run.type == "QuasiUnsteady") {
    switch(RAS.version,
      "5.0.6" = file.path("Results", "Sediment", "Summary")
    )
  } else {
    switch(RAS.version,
      "5.0.3" = file.path("Results", "Steady", "Summary"),
      "5.0.4" = file.path("Results", "Steady", "Summary"),
      "5.0.5" = file.path("Results", "Steady", "Summary")
    )
  }
}

# path to plan metadata
get_plan_meta_table = function(run.type, RAS.version) {
  list(
    "Plan Data/Plan Information",
    "Plan Data/Plan Parameters"
  )
}

# path to 2D area coordinates for results data
get_2d_coordinates_table = function(RAS.version, name, loctype) {
  if (loctype == "FACE")
    stop("face coordinates not implemented")
  else if (loctype == "FACEPOINT")
    get_coordinates_facepoint(RAS.version, name)
  else if(loctype == "CELL")
    get_coordinates_cell(RAS.version, name)
  else
    stop("Location type ", loctype, " not recognized.")
}

# path to 2D area facepoint coordinates
get_coordinates_facepoint = function(RAS.version, name) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "2D Flow Areas", name, 
      "FacePoints Coordinate"),
    "5.0.4" = file.path("Geometry", "2D Flow Areas", name, 
      "FacePoints Coordinate"),
    "5.0.5" = file.path("Geometry", "2D Flow Areas", name, 
      "FacePoints Coordinate")
  )
}

# path to 2D area cell center coordinates
get_coordinates_cell = function(RAS.version, name) {
  switch(RAS.version,
    "5.0.3" = file.path("Geometry", "2D Flow Areas", name, 
      "Cell Center Coordinate"),
    "5.0.4" = file.path("Geometry", "2D Flow Areas", name, 
      "Cell Center Coordinate"),
    "5.0.5" = file.path("Geometry", "2D Flow Areas", name, 
      "Cell Center Coordinate")
  )
}  
