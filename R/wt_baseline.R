#' @title Standard Deviation Non-Wear
#' @description Calculate non-wear time from raw acceleration data by finding intervals in which the acceleration standard deviation is below a std_threshold value

#'
#' @param df activity data, usually output from [read.gt3x::read.gt3x()],
#' and then imputed, or a data.frame of time/X/Y/Z
#' @param sample_rate sample rate (integer) of the sampling frequency in Hertz from the header
#' @param verbose print diagnostic messages
#' @param std_threshold standard deviation threshold in g
#' @param min_interval minimum interval (in minutes) that determines non-wear
#' @param use_vmu Use vector magnitude
#' @param verbose print diagnostic messages
#'
#' @note See \url{https://doi.org/10.1101/2020.07.08.20148015}
#' @return A tibble of time and indicator of wear
#' @export
#'
#' @examples
#' if (isTRUE(wt_packages_installed())) {
#'   path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#'                      package = "weartime")
#'   df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
#'                             imputeZeroes = TRUE,
#'                             verbose = TRUE)
#'   out = wt_baseline(df)
#' }
wt_baseline = function(df,
                       sample_rate = NULL,
                       std_threshold = 0.004,
                       min_interval = 90L,
                       use_vmu = FALSE,
                       verbose = TRUE) {
  check_py_packages()

  df = standardize_data(df)
  sample_rate = get_sample_rate(df, sample_rate, verbose)
  times = df$time
  df$time = NULL
  df = as.matrix(df)
  df = reticulate::np_array(df)

  stopifnot(!is.null(sample_rate))

  import_path = system.file(
    "extdata", "cnn",
    package = "weartime")
  wear = reticulate::import_from_path(
    "functions", path =  import_path,
    convert = TRUE)


  std_threshold = as.numeric(std_threshold)
  min_interval = as.integer(min_interval)
  use_vmu = as.logical(use_vmu)
  sample_rate = as.integer(sample_rate)
  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )

  out = wear$raw_non_wear_functions$raw_baseline_calculate_non_wear_time(
    raw_acc = df,
    hz = sample_rate,
    std_threshold = std_threshold,
    min_interval = min_interval,
    use_vmu = use_vmu,
    nwt_encoding = 0L,
    wt_encoding = 1L)

  out = c(out)
  stopifnot(length(out) == nrow(df))

  if (!is.null(times)) {
    out = tibble::tibble(time = times, wear = out > 0)
  } else {
    out = tibble::tibble(wear = out > 0)
  }
  return(out)

}

#' @rdname wt_baseline
#' @export
wt_xyz = wt_baseline

#' @rdname wt_baseline
#' @param ... not used
#' @export
wt_vmu = function(...,
                  min_interval = 105L,
                  use_vmu = TRUE
) {
  wt_baseline(...,
              min_interval = min_interval,
              use_vmu = use_vmu)

}
