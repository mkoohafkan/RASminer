#' Extract Cross Section Data
#'
#' Read the cross section data output.
#'
#' @inheritParams mine_hydraulic
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data; column "Station" containing the station ID in format "XS_####"
#'   where ### is the cross-section ID; column "Distance" containing the lateral
#'   distance location; and column "Elevation" containing the elevation at the
#'   distance location.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' 
#' read_xs(simple.quasi) 
#' read_xs(simple.quasi, which.times = "02DEC1990 01:00:00",
#'   which.stations = c("XS_800", "XS_796.00*"))
#' read_xs(simple.quasi, which.times = 2:4, which.stations = 1:3)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom stringr str_c str_split_fixed str_subset str_sub
#' @export
mine_xs = function(f, which.times = NULL, which.stations = NULL) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }
  # get run type
  ras.version = get_RAS_version()
  run.type = get_run_type()

  stations = list_output_stations()
  tblblock = get_xsection_block(run.type, ras.version)
  xsoutputs = list_xs(tblblock)

  if (is.null(which.times)) {
    which.times = str_split_fixed(xsoutputs, "[(.+)]", 3)[, 2]
  } else if (is.numeric(which.times)) {
    which.times = str_split_fixed(xsoutputs, "[(.+)]", 3)[, 2][which.times]

  } else if (any(nchar(which.times) != 18L)) {
    stop('Format of argument "which.times" not recognized')
  }
  xsoutputs = str_subset(xsoutputs, str_c(which.times, collapse = "|"))
  timeoutputs = str_sub(xsoutputs, 20, 37)
  times.found = which.times %in% timeoutputs
  if (!all(times.found)) {
    warning("The following times were not found in ", f, ": ",
    str_c(which.times[!times.found], collapse = ", "))
    which.times = which.times[times.found]
  }
  dlist = vector("list", length(which.times))
  for (i in seq_along(which.times)) {
    this.time = which.times[i]
    xs = str_subset(xsoutputs, this.time)
    if (length(xs) != 1L)
      stop("multiple tables named ", xs)
    index.table = file.path(tblblock, str_c(xs, " info"))
    values.table = file.path(tblblock, str_c(xs, " values"))
    xs.indices = rep(stations, get_dataset(index.table)[, 2])
    xs.table = get_dataset(values.table)
    colnames(xs.table) = c("Distance", "Elevation")
    xs.table = as_tibble(xs.table)
    xs.table["Station"] = xs.indices
    xs.table["Time"] = this.time
    dlist[[i]] = filter(xs.table, .data$Station %in% which.stations)
  }
  do.call(bind_rows, dlist)[c("Time", "Station", "Distance", "Elevation")]
}


#' Read Bed Limits
#'
#' Read the moveable bed limit stations.
#'
#' @inheritParams mine_hydraulic
#' @return a dataframe with columns "Time", "Station", "LOB" and "ROB", where
#'   columns "LOB" and "ROB" list the left and right moveable bed stations,
#'   respectively.
#'
#' @details The moveable bed limits will change over time in models that use
#'   BSTEM. In general, it is recommended but not required that the BSTEM toe
#'   stations are coincident with the moveable bed limits; otherwise, cross
#'   sections may adjust in unexpected ways.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' 
#' read_bed_limits(simple.quasi)
#' read_bed_limits(simple.quasi, which.times = "11DEC1990 01:00:00",
#'   which.stations = "XS_800")
#' read_bed_limits(simple.quasi, which.times = 1:4,
#'   which.stations = 1:2)
#'
#' @import stringr
#' @import dplyr
#' @export
read_bed_limits = function(f, which.stations = NULL,
  which.times = NULL) {
  # get run type
  run.type = get_run_type(f)
  # get bank station data
  lob = mine_hydraulic(f, "Moveable Sta L", which.times = which.times,
    which.stations = which.stations)
  rob = mine_hydraulic(f, "Moveable Sta R", which.times = which.times,
    which.stations = which.stations)
  list(LOB = lob, ROB = rob) %>%
    lapply(to_longtable, data.col = "Distance") %>%
    bind_rows(.id = "section") %>%
    to_widetable("section", "Distance") %>%
    arrange_("Time", "desc(Station)")
}

#' Read Bed Limit Elevations
#'
#' Read the elevations of the moveable bed limit stations.
#'
#' @inheritParams mine_hydraulic
#' @return a dataframe with columns "Time", "Station", "LOB" and "ROB", where
#'   columns "LOB" and "ROB" list the left and right moveable bed limit
#'   elevations, respectively.
#'
#' @details The moveable bed limit elevations will change over time, both due
#'   to channel change and due to lateral movement in models that use
#'   BSTEM.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' 
#' read_bed_limits_elev(simple.quasi)
#' read_bed_limits_elev(simple.quasi, which.times = "11DEC1990 01:00:00",
#'   which.stations = "XS_800")
#' read_bed_limits_elev(simple.quasi, which.times = 1:4,
#'   which.stations = 1:2)
#'
#' @import stringr
#' @import dplyr
#' @export
read_bed_limits_elev = function(f, which.stations = NULL,
  which.times = NULL) {
  # get run type
  # get bank station data
  lob = mine_hydraulic(f, "Moveable Elv L", which.times = which.times,
    which.stations = which.stations)
  rob = mine_hydraulic(f, "Moveable Elv R", which.times = which.times,
    which.stations = which.stations)
  list(LOB = lob, ROB = rob) %>%
    lapply(to_longtable, data.col = "Elevation") %>%
    bind_rows(.id = "section") %>%
    to_widetable("section", "Elevation") %>%
    arrange_("Time", "desc(Station)")
}


#' Read River Station Lengths
#'
#' Read the river station lengths.
#'
#' @inheritParams mine_hydraulic
#' @return a dataframe with columns "Station", "LOB", "Channel" and "ROB", where
#'   column "Channel" list the channel length and columns "LOB" and "ROB" list
#'   the left and right bank stations lengths, respectively.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' 
#' read_station_lengths(simple.quasi)
#'
#' @import stringr
#' @import dplyr
#' @export
read_station_lengths = function(f) {
  station.lengths = list_lengths(f)
  river.stations = list_stations(f)
  data_frame(Station = str_c("XS_", river.stations), LOB = station.lengths[,1],
    Channel = station.lengths[,2], ROB = station.lengths[,3])
}

#' Read Bank Stations
#'
#' Read the bank stations.
#'
#' @inheritParams mine_hydraulic
#' @return A table with columns "Station", "LOB" and "ROB", where
#'   columns "LOB" and "ROB" list the left and right bank stations,
#'   respectively.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#'  
#' read_bank_stations(simple.quasi)
#'
#' @importFrom tibble tibble
#' @importFrom stringr str_c
#' @export
read_bank_stations = function(f){
  bank.stations = list_bank_stations(f)
  river.stations = list_stations(f)
  tibble(Station = str_c("XS_", river.stations),
    LOB = bank.stations[,1], ROB = bank.stations[,2])
}
