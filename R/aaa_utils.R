get_sample_rate = function(df, sample_rate = NULL, verbose = TRUE) {
  verbose_message <- function(..., verbose = TRUE) {
    if (verbose) {
      message(...)
    }
  }
  if (is.null(sample_rate)) {
    sample_rate = attr(df, "sample_rate")
    if (!is.null(sample_rate)) {
      verbose_message(
        paste0("Getting sample_rate: ", sample_rate,
               " from attributes"),
        verbose = verbose)
    }
  }
  if (is.null(sample_rate)) {
    msg = "sample_rate must be specified!"
    if (is.null(df$time)) {
      stop(msg)
    }
    tdiff = as.numeric(diff(df$time))
    tdiff = round(tdiff, 4)
    tdiff = 1/unique(tdiff)
    if (length(tdiff) != 1) {
      stop(msg)
    }
    sample_rate = round(tdiff)
    verbose_message(
      paste0("sample_rate estimated to be ",
             sample_rate, ". If not correct, ", msg),
      verbose = verbose)
  }
  return(sample_rate)
}

is.AccData = function(x) {
  inherits(x, "AccData")
}
