#' Mine Hydraulic Data
#'
#' Extract hydraulic or other non-sediment data.
#'
#' @inheritParams get_tables
#' @param table.name The table to read.
#' @param which.times Character vector of timestamps to extract. If
#'   NULL, all timestamps will be returned.
#' @param which.stations Character vector of stations to extract. If
#'   NULL, all stations will be returned.
#' @param override.sediment (For Unsteady+Sediment models only) 
#'   If `TRUE`, extract data from the hydraulic rather than sediment 
#'   output.
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data and columns "XS_####" where ### is the cross-section ID.
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' mine_hydraulic(simple.quasi, "Flow")
#'
#' simple.unsteady = system.file("sample-data/SampleUnsteady.hdf",
#'   package = "RAStestR")
#' mine_hydraulic(simple.unsteady, "Flow")
#'
#' mine_hydraulic(simple.quasi, "Flow", which.times = "11DEC1990 01:00:00",
#'   which.stations = c("XS_800", "XS_796.00*"))
#'
#' mine_hydraulic(simple.quasi, "Flow", which.times = 2:3,
#'   which.stations = 1:4)
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @importFrom stringr str_replace str_c str_detect
#' @importFrom tibble tidy_names
#' @importFrom tidyr gather
#' @export
mine_hydraulic = function(f, table.name, which.times = NULL,
  which.stations = NULL, override.sediment = FALSE) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

  # get run type
  run.type = get_run_type()
  ras.version = get_RAS_version()
  if (run.type == "Unsteady+Sediment" && override.sediment)
    run.type == "Unsteady"
  # argument checks
  output.times = list_output_times()
  if (is.null(which.times))
    which.times = seq_along(output.times)
  else if (!is.numeric(which.times))
    which.times = which(output.times %in% which.times)
  else
    which.times = which(seq_along(output.times) %in% which.times)
  if (length(which.times) < 1L)
    stop("No data matching 'which.times' was found")
  stations = list_output_stations()
  if (is.null(which.stations))
    which.stations = seq_along(stations)
  else if (!is.numeric(which.stations))
    which.stations = which(stations %in% str_replace(which.stations, 
      "XS_", ""))
  else
    which.stations = which(seq_along(stations) %in% which.stations)
  if (length(which.stations) < 1L)
    stop("No data matching 'which.stations' was found")
  #generate station labels
  station.labels = stations
  # warn about duplicates
  if (any(duplicated(station.labels[which.stations]))) {
    warning("Duplicate station labels detected: ", paste(
      station.labels[which.stations][duplicated(station.labels[which.stations])],
      collapse = ", "), call. = FALSE)
    station.labels = tidy_names(station.labels)
  }
  # specify tables
  tblpath = file.path(get_output_block(run.type, ras.version), table.name)
  # read data
  res = get_tables(tblpath, "Time", output.times,
    station.labels)[[1]]
  # filter by time/station
  othercols = setdiff(names(res), station.labels)
  stationcols = station.labels[which.stations]
  res = res[which.times, c(othercols, stationcols)]
  # return result
  gather(res, Station, !!table.name, !!stationcols)
}

#' Mine Sediment Data
#'
#' Extract sediment data.
#'
#' @inheritParams mine_hydraulic
#' @param which.grains Grain classes to extract data for. Can accept 
#'   either numeric grain class IDs or grain class labels. Label "ALL" 
#'   or "" corresponds to the totals. If NULL, all grain classes will 
#'   be returned.
#' @return A dataframe with a column "Time" containing the Date Time
#'   Stamp data; columns "XS_####" where ### is the cross-section ID;
#'   and column "GrainClass".
#'
#' @examples
#' simple.quasi = system.file("sample-data/SampleQuasiUnsteady.hdf",
#'   package = "RAStestR")
#' mine_sediment(simple.quasi, "Vol Out Cum")
#'
#' mine_sediment(simple.quasi, "Vol Out Cum", 
#'   which.grains = c("6", "7"))
#' mine_sediment(simple.quasi, "Vol Out Cum", 
#'   which.grains = c("VFS", "FS"))
#'
#' @import hdf5r
#' @import tibble
#' @import dplyr
#' @import stringr
#' @export
mine_sediment = function(f, table.name, which.times = NULL,
  which.stations = NULL, which.grains = NULL) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

  # nse workaround
  GrainClass = NULL
  # get run type
  run.type = get_run_type()
  ras.version = get_RAS_version()
  # argument checks
  grain.labels = list_grain_classes()
  grain.levels = c("", paste(1:20))
  if (!is.null(which.grains)) {
    given.grains = which.grains
    if (any(which.grains %in% grain.labels)) {
      error.grains = setdiff(which.grains, grain.labels)
      if (length(error.grains) > 0L)
        stop("Grain class ID not recognized: ",
          paste(error.grains, collapse =", "))
      which.grains = grain.levels[grain.labels %in% which.grains]
    } else if (any(which.grains %in% grain.levels)) {
      error.grains = setdiff(which.grains, grain.levels)
      if (length(error.grains) > 0L)
        stop("Grain class ID not recognized: ",
          paste(error.grains, collapse =", "))
      which.grains = grain.levels[grain.levels %in% which.grains]
    } else {
      stop("No data matching 'which.grains' was found")
    }
  } else {
    which.grains = grain.levels
  }
  output.times = list_output_times()
  if (is.null(which.times)) {
    which.times = seq_along(output.times)
  } else if (!is.numeric(which.times)) {
    which.times = which(output.times %in% which.times)
  } else {
    which.times = which(seq_along(output.times) %in% which.times)
  }
  if (length(which.times) < 1L)
    stop("No data matching 'which.times' was found")
  stations = list_output_stations()
  if (is.null(which.stations)) {
    which.stations = seq_along(stations)
  } else if (!is.numeric(which.stations)) {
    which.stations = which(stations %in% str_replace(which.stations, 
      "XS_", ""))
  } else {
    which.stations = which(seq_along(stations) %in% which.stations)
  }
  if (length(which.stations) < 1L)
    stop("No data matching 'which.stations' was found")
  # get sediment tables
  sedimentpath = file.path(get_sediment_block(run.type, ras.version),
    table.name)
  included.grains = str_trim(str_replace(list_sediment(sedimentpath),
    sedimentpath, ""))
  if (length(included.grains) < 1)
    stop('Table "', sedimentpath, '" could not be found.', call. = FALSE)
  missing.grains = setdiff(which.grains, included.grains)
  selected.grains = intersect(which.grains, included.grains)  
  table.paths = str_trim(str_c(sedimentpath, selected.grains, sep = " "))
  table.labels = grain.labels[grain.levels %in% selected.grains]
  #generate station labels
  station.labels = stations
  # warn about duplicates
  if (any(duplicated(station.labels[which.stations]))) {
    warning("Duplicate station labels detected: ", paste(
      station.labels[which.stations][duplicated(station.labels[which.stations])],
      collapse = ", "), call. = FALSE)
  }
  # read in data
  res.list = get_tables(table.paths, "Time", output.times,
    station.labels)
  res.list = lapply(res.list, function(tbl) tbl[which.times,])
  names(res.list) = table.labels
  res = mutate(bind_rows(res.list, .id = "GrainClass"),
    GrainClass = factor(GrainClass, levels = grain.labels))
  othercols = setdiff(names(res), station.labels)
  stationcols = station.labels[which.stations]
  res = res[c(othercols, stationcols)]
  # return result
  gather(res, Station, !!table.name, !!stationcols)
}
