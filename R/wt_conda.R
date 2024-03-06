#' Create Conda Environment for `weartime`
#'
#' @param envname conda environment name for the modules to be installed
#' @param ... additional arguments to pass to [reticulate::conda_create()]
#'
#' @return Nothing, called for side effects
#' @export
conda_create_weartime = function(envname = "weartime", ...) {
  args = list(...)
  args$envname = envname
  packages = c("numpy",
               "pandas",
               "joblib",
               "psutil",
               "glob2",
               "tensorflow>=2.0.0",
               "bitstring",
               "scipy",
               "resampy")
  packages = c(packages, args$packages)
  args$packages = NULL
  do.call(reticulate::conda_create, args = args)
  reticulate::py_install(packages = packages, pip = TRUE,
                         envname = envname,
                         method = "conda",
                         ignore_installed = TRUE,
                         pip_ignore_installed = TRUE)
  invisible(NULL)
}

#' @rdname conda_create_weartime
#' @export
use_weartime_condaenv = function(envname = "weartime") {
  reticulate::use_condaenv(condaenv = envname, required = TRUE)
}

#' @export
#' @rdname conda_create_weartime
unset_reticulate_python = function() {
  Sys.unsetenv("RETICULATE_PYTHON")
}

#' @rdname conda_create_weartime
#' @export
have_weartime_condaenv = function(envname = "weartime") {
  reticulate::condaenv_exists(envname = envname)
}

module_version = function(module = "numpy") {
  assertthat::is.scalar(module)
  if (!reticulate::py_module_available(module)) {
    stop(paste0(module, " is not installed!"))
  }
  df = reticulate::py_list_packages()
  df$version[df$package == module]
}

