RAStestR_default_options = list(
    RAStestR.DefaultVersion = "5.0.3",
    RAStestR.ForceVersion = NULL,
    RAStestR.VersionOverride = NULL
)

.onLoad = function(libname, pkgname) {
  op = options()
  toset = !(names(RAStestR_default_options) %in% names(op))
  if (any(toset))
    options(RAStestR_default_options[toset])
  invisible(NULL)
}

#' @importFrom hdfqlr hql_load
.onAttach = function(libname, pkgname) {
    tryCatch(hql_load(),
      error = function(e)
        packageStartupMessage('Connect to your HDFql library ',
        'by calling "hdfqlr::hql_load()".'))
}

.onDetach = function(libpath) {
  message('If you are finished using your HDFql library, ',
    'detach it by calling "hdfqlr::hql_unload()".')
}
