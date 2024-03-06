#' 	Estimation of non-wear time periods based on Hees 2013 paper
#'
#' @param df activity data, usually output from [read.gt3x::read.gt3x()],
#' and then imputed, or a data.frame of time/X/Y/Z
#' @param sample_rate sample rate (integer) of the sampling frequency in Hertz from the header
#' @param verbose print diagnostic messages
#' @param min_non_wear_time_window int (optional) minimum window length in minutes to be classified as non-wear time
#' @param window_overlap int (optional) basically the sliding window that progresses over the acceleration data. Defaults to 15 minutes.
#' @param std_threshold float (optional) standard deviation threshold in g. Acceleration axes values below or equal this threshold can be considered non-wear time. Defaults to 3.0mg.
#' @param std_min_num_axes int (optional)  minimum number of axes used to check if acceleration values are below the `std_threshold` value. Defaults to 2 axes; meaning that at least 2  axes need to have values below a threshold value to be considered non wear time
#' @param value_range_threshold float (optional) value range threshold value in g. If the range of values within a window is below this threshold (meaning that there is very little change in acceleration over time) then this can be considered non wear time. Default to 50 mg.
#' @param value_range_min_num_axes int (optional) minimum number of axes used to check if acceleration values range are below the value_range_mg_threshold value. Defaults to 2 axes; meaning that at least 2 axes need to have a value range below a threshold value to be considered non wear time
#'
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
#'   out = wt_hees_2013(df)
#' }
#' \dontrun{
#'
#' url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
#' destfile = tempfile(fileext = ".zip")
#' dl = download.file(url, destfile = destfile)
#' gt3x_file = unzip(destfile, exdir = tempdir())
#' gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
#' path = gt3x_file
#'
#' df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
#'                            imputeZeroes = TRUE,
#'                            verbose = TRUE)
#' out = wt_hees_2013(df)
#' df = resample_acc(df)
#' out_100 = wt_hees_2013(df)
#' }
wt_hees_2013 =  function(
  df,
  sample_rate = NULL,
  min_non_wear_time_window = 60L,
  window_overlap = 15L,
  std_threshold = 0.003,
  std_min_num_axes = 2L,
  value_range_threshold = 0.050,
  value_range_min_num_axes = 2L,
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


  window_overlap = as.integer(window_overlap)
  value_range_min_num_axes = as.integer(value_range_min_num_axes)
  std_min_num_axes = as.integer(std_min_num_axes)
  std_threshold = as.numeric(std_threshold)
  value_range_threshold = as.numeric(value_range_threshold * 1000)
  # need to do this so that it's the same across all functions.
  std_threshold = as.numeric(std_threshold * 1000)

  out = wear$raw_non_wear_functions$hees_2013_calculate_non_wear_time(
    data = df,
    hz = as.integer(sample_rate),
    std_mg_threshold = as.numeric(std_threshold),
    min_non_wear_time_window = min_non_wear_time_window,
    window_overlap = window_overlap,
    std_min_num_axes = std_min_num_axes,
    value_range_mg_threshold = value_range_threshold,
    value_range_min_num_axes = value_range_min_num_axes,
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





#' @rdname wt_hees_2013
#' @param ... not used
#' @export
wt_hees_2011 =  function(
  ...,
  min_non_wear_time_window = 30L) {
  wt_hees_2013(..., min_non_wear_time_window = min_non_wear_time_window)
}

#' @rdname wt_hees_2013
#' @export
wt_hees_optimized = function(
    df,
    sample_rate = NULL,
    min_non_wear_time_window = 135L,
    window_overlap = 15L,
    std_threshold = 0.007,
    std_min_num_axes = 1L,
    value_range_threshold = 0.001,
    value_range_min_num_axes = 1L,
    verbose = TRUE) {
    wt_hees_2013(
      df = df,
      sample_rate = sample_rate,
      min_non_wear_time_window = min_non_wear_time_window,
      window_overlap = window_overlap,
      std_threshold = std_threshold,
      std_min_num_axes = std_min_num_axes,
      value_range_threshold = value_range_threshold,
      value_range_min_num_axes = value_range_min_num_axes,
      verbose = verbose)
  }
