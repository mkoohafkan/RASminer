#' Get RAS Version
#'
#' Get the RAS version that generated the specified file.
#' @inheritParams get_tables
#' @return The RAS version.
#'
#' @importFrom hdfqlr hql_use_file hql_close_file
#' @export
get_RAS_version = function(f) {
  if (!is.null(options()[["RASminer.ForceVersion"]]))
    v = options()[["RASminer.ForceVersion"]]
  else {
    if (!missing(f)) {
      hql_use_file(f)
      on.exit(hql_close_file(f))
    }
    tryCatch({
      meta.attr = get_group_attr()
    }, error = function(e) {
      warning(e)
      stop("Could not find RAS metadata", call. = FALSE)
    }
  )
    v = str_split(meta.attr[["File Version"]], " ", simplify = TRUE)[2]
    if (v %in% names(options()[["RASminer.VersionOverride"]]))
      options()[["RASminer.VersionOverride"]][[v]]
    }

  if (!(v %in% supported_RAS_versions()))
    stop("RAS version ", v, " is not currently supported", call. = FALSE)
  v
}

#' Force RAS Version
#'
#' Force RASminer to assume the specified RAS version. Useful for
#' working with RAS development environments or incomplete datasets 
#' that do not include RAS version info.
#'
#' @param version The RAS version to use.
#'
#' @export
force_RAS_version = function(version) {
  version = match.arg(version, supported_RAS_versions())
  if (missing(version))
    return(options()$RASminer.RASversion)
  options(RASminer.ForceVersion = version)
  invisible(version)
}

#' Override RAS Version
#'
#' Specify RAS version overrides. Useful when working with files produced 
#' from both RAS development and release versions, e.g. by assuming that
#' files produced with RAS development x.x are structured in the same way
#' as a specific release version.
#'
#' @param v The RAS version to override.
#' @param override.v The RAS version to be assumed.
#'
#' @export
override_RAS_version = function(v, override.v) {
  ops = options()[["RASminer.VersionOverride"]]
  ops[[v]] = override.v
  options(RASminer.VersionOverride = ops)
}

supported_RAS_versions = function() {
  c(
    "5.0.3", 
    "5.0.4", 
    "5.0.5"
  )
}
