#' Install Packages Needed for `weartime`
#'
#' @param packages list of packages, passed to [reticulate::py_install()]
#' @param ... additional arguments passed to passed to [reticulate::py_install()]
#'
#' @return Output from [reticulate::py_install()]
#' @export
wt_install_packages = function(
    packages = c("numpy",
                 "pandas",
                 "joblib",
                 "psutil",
                 "glob2",
                 "tensorflow==2.0.0",
                 "bitstring",
                 "scipy",
                 "resampy",
                 "csv",
                 "logging",
                 "time",
                 "datetime"),
    ...
) {
  reticulate::py_install(packages = packages, ...)
}

check_py_packages = function(
    packages = c("numpy",
                 "pandas",
                 "joblib",
                 "psutil",
                 "glob2",
                 "tensorflow",
                 "bitstring",
                 "scipy",
                 "resampy",
                 "csv",
                 "logging",
                 "time",
                 "datetime")
) {
  n = names(packages)
  names(packages)[n == ""] = packages[n == ""]
  py_avail = reticulate::py_available(initialize = FALSE)
  if (!py_avail) {
    warning("reticulate::py_available indicated python not available, ",
            "still running")
    return(NULL)
  }
  sapply(packages, reticulate::py_module_available)
  res = sapply(packages, reticulate::py_module_available)

  if (any(!res)) {
    no_pkg = names(res)[!res]
    tfile = tempfile()
    dput(no_pkg, file = tfile)
    np = readLines(tfile)
    msg = paste0(paste(no_pkg, collapse = ", "),
                 " packages not found, please try",
                 " to install using reticulate::py_install(",
                 np, ")\n",
                 "otherwise, may not work")
    warning(msg)
  }
  return(all(res))
}
