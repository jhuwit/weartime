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
  reticulate::py_available(initialize = TRUE)
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
