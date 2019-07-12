#' List 2D Area Coordinates
#'
#' list the face point or cell center coordinates of a 2D area.
#'
#' @inheritParams get_tables
#' @param which.area The 2D area to get the coordinates for.
#' @param type The type of coordinates to get, i.e. `FacePoint`
#'  or `Cell` (center) coordinates.
#' @return a dataframe of XY coordinates for the 2D area.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_to_upper
#' @export
list_2d_coordinates = function(which.area, type = c("FacePoint", "Cell"), f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  type = str_to_upper(type)
  RAS.version = get_RAS_version()
  coords.table = get_2d_coordinates_table(RAS.version, which.area, type)
  get_dataset(coords.table)
}

#' List Plan Information
#'
#' List basic plan information.
#'
#' @inheritParams get_tables
#' @param print Print the plan information to the console in a nice format.
#' @return A list of plan information.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_c
#' @export
list_plan_info = function(print = TRUE, f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  which.attr = c("Plan Name", "Plan ShortID", "Type of Run",
    "Time Window", "Computation Time Step", "Output Interval")
  metadata = get_plan_meta()[which.attr]
  if (print) {
    message(str_c(names(metadata), unlist(metadata),
      sep = ": ", collapse = "\n"), "\n")
  }
  invisible(metadata)
}

#' List Grain Classes
#'
#' List sediment grain class labels.
#'
#' @inheritParams get_tables
#' @return a vector of grain glass labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_grain_classes(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_grain_classes = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  run.type = get_run_type()
  ras.version = get_RAS_version()
  if (!(run.type %in% c("QuasiUnsteady", "Unsteady+Sediment"))) {
    stop(sprintf('No sediment data available for run type "%s"',
      run.type))
  }
  grainpath = get_grain_class_table(ras.version)
  c("ALL", str_trim(get_dataset(grainpath)))
}

#' List Output Times
#'
#' List output times.
#'
#' @inheritParams get_tables
#' @return a vector of cross section lengths.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_output_times(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_output_times = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  run.type = get_run_type()
  ras.version = get_RAS_version()
  timespath = get_timestep_table(run.type, ras.version)
  str_trim(get_dataset(timespath))
}


#' List Cross Section Lengths
#'
#' List cross section lengths.
#'
#' @inheritParams get_tables
#' @return a matrix of cross section lengths.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_lengths(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @export
list_lengths = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

  ras.version = get_RAS_version()
  lengthpath = get_lengths_table(ras.version)
  switch(ras.version,
    "5.0.3" = get_dataset(lengthpath),
    "5.0.4" = setNames(as.matrix(
      get_dataset(lengthpath)[c("Len Left", "Len Channel", "Len Right")]
      ), NULL),
    "5.0.5" = setNames(as.matrix(
      get_dataset(lengthpath)[c("Len Left", "Len Channel", "Len Right")]
      ), NULL)
  )
}

#' List Bank Stations
#'
#' List bank stations.
#'
#' @inheritParams get_tables
#' @return a vector of bank stations
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_bank_stations(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stats setNames
#' @export
list_bank_stations = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  bankpath = get_bank_stations_table(ras.version)
  switch(ras.version,
    "5.0.3" = get_dataset(bankpath),
    "5.0.4" = setNames(as.matrix(
      get_dataset(bankpath)[c("Left Bank", "Right Bank")]
      ), NULL),
    "5.0.5" = setNames(as.matrix(
      get_dataset(bankpath)[c("Left Bank", "Right Bank")]
      ), NULL)
  )
}

#' List Levees
#'
#' List levee stations and elevations.
#'
#' @inheritParams get_tables
#' @return a matrix of levee stations and elevations. Column order is:
#'   left station, left elevation, right station, right elevation.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stats setNames
#' @export
list_levees = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  leveepath = get_levees_table(ras.version)
  switch(ras.version,
    "5.0.3" = get_dataset(leveepath),
    "5.0.4" = setNames(as.matrix(
      get_dataset(leveepath)[c("Left Levee Sta", "Left Levee Elev",
        "Right Levee Sta", "Right Levee Elev")]
      ), NULL),
    "5.0.5" = setNames(as.matrix(
      get_dataset(leveepath)[c("Left Levee Sta", "Left Levee Elev",
        "Right Levee Sta", "Right Levee Elev")]
      ), NULL)
  )
}

#' List River Stations
#'
#' List river station labels.
#'
#' @inheritParams get_tables
#' @return a vector of river station labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_stations(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_stations = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  stationpath = get_station_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(stationpath)),
    "5.0.4" = str_trim(get_dataset(stationpath)[["RS"]]),
    "5.0.5" = str_trim(get_dataset(stationpath)[["RS"]])
  )
}

#' List Output Stations
#'
#' List output station labels.
#'
#' @inheritParams get_tables
#' @return a vector of river station labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_output_stations(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_output_stations = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  run.type = get_run_type()
  ras.version = get_RAS_version()
  stationpath = get_output_station_table(run.type, ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(stationpath)),
    "5.0.4" = str_trim(get_dataset(stationpath)[["RS"]]),
    "5.0.5" = str_trim(get_dataset(stationpath)[["RS"]])
  )
}

#' List Node Names
#'
#' List node names.
#'
#' @inheritParams get_tables
#' @return a vector of node names.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_node_names = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  nodepath = get_nodename_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(nodepath)),
    "5.0.4" = str_trim(get_dataset(nodepath)[["Node Name"]]),
    "5.0.5" = str_trim(get_dataset(nodepath)[["Node Name"]])
  )
}

#' List Node Descriptions
#'
#' List node descriptions.
#'
#' @inheritParams get_tables
#' @return a vector of node descriptions.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_node_descriptions = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  descpath = get_nodedesc_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(descpath)),
    "5.0.4" = str_trim(get_dataset(descpath)[["Desc"]]),
    "5.0.5" = str_trim(get_dataset(descpath)[["Desc"]])
  )
}


#' List River Names
#'
#' List river names.
#'
#' @inheritParams get_tables
#' @return a vector of river names.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_rivers(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_rivers = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  riverpath = get_river_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(riverpath)),
    "5.0.4" = str_trim(get_dataset(riverpath)[["River"]]),
    "5.0.5" = str_trim(get_dataset(riverpath)[["River"]])
  )
}

#' List Reach Names
#'
#' List reach names.
#'
#' @inheritParams get_tables
#' @return a vector of reach names.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' RAStestR:::list_reaches(simple.quasi)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_reaches = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  reachpath = get_reach_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(reachpath)),
    "5.0.4" = str_trim(get_dataset(reachpath)[["Reach"]]),
    "5.0.5" = str_trim(get_dataset(reachpath)[["Reach"]])
  )
}



#' List Sediment Tables
#'
#' List grain class-specific tables of the specified type.
#'
#' @inheritParams get_tables
#' @return a vector of grain glass labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' list_sediment(simple.quasi, file.path("Results", "Sediment", 
#'     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#'     "Vol In"))
#' list_sediment(simple.quasi, file.path("Results", "Sediment", 
#'     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#'     "Vol Inactive"))
#' list_sediment(simple.quasi, file.path("Results", "Sediment", 
#'     "Output Blocks", "Sediment", "Sediment Time Series", "Cross Sections", 
#'     "Vol In Cum"))
#'
#' @importFrom hdfqlr hql_use_file hql_close_file hql_list_datasets
#' @importFrom stringr str_subset
#' @keywords internal
list_sediment = function(table.name, f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

  group.x = hql_list_datasets(dirname(table.name))
  str_subset(group.x,
    sprintf("%s\\b(\\s\\d+)*$", basename(table.name)))
}

#' List Cross Section Tables
#'
#' List cross section tables.
#'
#' @inheritParams get_tables
#' @param xs.block The HDF group containing the cross section output tables.
#' @return a vector of cross section output labels.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' list_xs(simple.quasi, file.path("Results", "Sediment",
#'     "Output Blocks", "Sediment SE", "Sediment Time Series", 
#'     "Cross Section SE"))
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @keywords internal
list_xs = function(xs.block, f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  tblpath = file.path(dirname(xs.block), "Time Date Stamp")
  unique(sprintf("Station Elevation (%s)",
    str_trim(get_dataset(tblpath))))
}

#' List 2D Areas
#'
#' List 2D flow areas
#'
#' @inheritParams get_tables
#' @return a vector of 2D flow area names.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_trim
#' @export
list_2dareas = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  ras.version = get_RAS_version()
  flowareapath = get_2d_flow_area_table(ras.version)
  switch(ras.version,
    "5.0.3" = str_trim(get_dataset(flowareapath)),
    "5.0.4" = str_trim(get_dataset(flowareapath)[["Name"]]),
    "5.0.5" = str_trim(get_dataset(flowareapath)[["Name"]])
  )
}
