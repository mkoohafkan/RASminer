#' Read HDF Dataset
#'
#' Safely read in a specific HDF table. This function is used 
#' internally and should not be called directly by the user.
#'
#' @param path The table path.
#'
#' @importFrom hdfqlr hql_read_dataset
#' @keywords internal
get_dataset = function(path) {
  tryCatch(
    hql_read_dataset(path),
    error = function(e) {
      stop("Could not read dataset ", path, 
        "\nAdditional Info:\n  ", e, call. = FALSE)
    }
  )
}

#' Read HDF Attributes
#'
#' Safely read attributes of an HDF Group. This function is used 
#' internally and should not be called directly by the user.
#'
#' @inheritParams get_dataset
#' @param attrs The attributes to read. If \code{NULL}, all 
#'   attributes will be read.
#' @return a named list of attributes.
#'
#' @importFrom hdfqlr hql_list_attributes hql_read_attribute
#' @keywords internal
get_group_attr = function(path = "", attrs = NULL) {
  if (is.null(attrs)) {
    attrs = tryCatch(hql_list_attributes(path),
      error = function(e) {
        stop("Could not list attributes of ", path,
          "\nAdditional Info:\n  ", e, call. = FALSE)
      })
  }
  group.attr = lapply(file.path(path, attrs), function(a)
    tryCatch(hql_read_attribute(a),
      error = function(e) {
        stop("Could not read attribute", path,
          "\nAdditional Info:\n  ", e, call. = FALSE)
      }))
  names(group.attr) = attrs
  group.attr
}

#' Read HEC-RAS Tables
#'
#' Read RAS sediment data output. This function is used internally and should
#' not be called directly by the user.
#'
#' @param f The HDF5 file to read.
#' @param table.path The table to read in.
#' @param rowcolname The name to assign to the new row id column.
#' @param rlabs A vector of row identifiers.
#' @param clabs A vector of column identifiers.
#' @return A tibble.
#'
#' @importFrom utils head tail
#' @importFrom hdfqlr hql_use_file hql_close_file hql_list_datasets
#' @importFrom tibble as_tibble
#' @importFrom stringr str_c
#' @keywords internal
get_tables = function(table.paths, rowcolname, rlabs, clabs, f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

#  all.tables = hql_list_datasets(recursive = TRUE)
#  missing.tables = setdiff(table.paths, all.tables)
#  if (length(missing.tables) > 0L) {
#    stop("Could not find datasets: ",
#      str_c(basename(missing.tables), collapse = ", "),
#      call. = FALSE)
#  }
  lapply(table.paths, function(pth) {
    this = get_dataset(pth)
    colnames(this) = clabs
    this = as_tibble(this)
    this[rowcolname] = rlabs
    this[c(rowcolname, clabs)]
  })
}


#' Get RAS Plan Run Type
#'
#' Identify a RAS plan as Unsteady, Steady or QuasiUnsteady.
#'
#' @inheritParams get_tables
#' @return The run type of the RAS plan.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file hql_list_groups
#' @keywords internal
get_run_type = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

  event.groups = tryCatch(
    basename(hql_list_groups("Event Conditions")),
    error = function(e) {
      warning(e)
      stop("Could not find Event Conditions", call. = FALSE)
    }
  )
  if ("QuasiUnsteady" %in% event.groups) {
    "QuasiUnsteady"
  } else if ("Unsteady" %in% event.groups) {
    if ("Sediment" %in% event.groups) {
      "Unsteady+Sediment"
    } else {
      "Unsteady"
    }
  } else if ("Steady" %in% event.groups) {
    "Steady"
  } else {
    stop("Could not find run type specification 'Steady', ",
    "'Unsteady' or 'QuasiUnsteady'")
  }
}


#' Get RAS Plan Meta Data
#'
#' Get plan meta data of the RAS plan.
#'
#' @inheritParams get_tables
#' @return A named list of plan meta data.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @keywords internal
get_plan_meta = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

  run.type = get_run_type()
  RAS.version = get_RAS_version()
  paths = get_plan_meta_table(run.type, RAS.version)
  tryCatch({
    plan.attr = c(
      get_group_attr(),
      get_group_attr(paths[[1]]),
      get_group_attr(paths[[2]])
    )
  }, error = function(e) {
    stop("Could not find Plan Data",
      "\nAdditional Info:\n  ", e, call. = FALSE)
  })
  plan.attr[["Type of Run"]] = run.type
  return(plan.attr)
}

#' Get RAS Results Meta Data
#'
#' Get meta data of the RAS results.
#'
#' @inheritParams get_tables
#' @return A named list of results meta data.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @keywords internal
get_results_meta = function(f) {
  if (!missing(f)) {
    hql_use_file(f)
    on.exit(hql_close_file(f))
  }

  run.type = get_run_type()
  RAS.version = get_RAS_version()
  path = get_results_meta_table(run.type, RAS.version)
  if (is.null(path)) {
    warning("Results metadata is not recorded in HEC-RAS ",
      RAS.version)
    return(invisible(NULL))
  }
  tryCatch({
    results.attr = get_group_attr(path)
  }, error = function(e) {
    stop("Could not find Results metadata",
      "\nAdditional Info:\n  ", e, call. = FALSE)
  })
  results.attr[["Type of Run"]] = run.type
  return(results.attr)
}
