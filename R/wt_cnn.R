#' Wear Time Estimation of Raw Data from a Convolutional Neural Network
#'
#' @details Detect candidate non-wear episodes:
#'   Perform a forward pass through the raw acceleration signal and calculate the SD for each 1-minute interval and for each individual axis.
#'   If the standard deviation is <= 0.004 g for all axes, record this 1-minute interval as a candidate non-wear interval. After all 1-minute
#'   intervals have been processed, merge consecutive 1-minute intervals into candidate non-wear episodes and record their start and stop timestamps.
#'
#' Merge bordering candidate non-wear episodes:
#'   Merge candidate non-wear episodes that are no more than 5 minutes apart and record their new start and stop timestamps. This step is required
#'   to capture artificial movement that would typically break up two or more candidate non-wear episodes in close proximity.
#'
#' Detect the edges of candidate non-wear episodes:
#'   Perform a backward pass with a 1-second step size through the acceleration data from the start timestamp of a candidate non-wear episode and
#'   calculate the SD for each individual axis. The same is applied for the stop timestamps with a forward pass and a step size of 1 second.
#'   If the standard deviation of all axes is  <= 0.004 g, include the 1-second interval into the candidate non-wear episode and record the new
#'   start or stop timestamp. Repeat until the standard deviation of the 1-second interval does not satisfy <= 0.004 g. As a result, the resolution
#'   of the edges is now recorded on a 1-second resolution.
#'
#' Classifying the start and stop windows:
#'   For each candidate non-wear episode, extract the start and stop segment with a window length of 3 seconds to create input features
#'   for the CNN classification model. For example, if a candidate non-wear episode has a start timestamp of tstart a feature matrix is
#'   created as (tstart – w  , tstart) x 3 axes with w = 3 seconds, resulting in an input feature with dimensions (300 x 3) for 100Hz data.
#'   If both (i.e., logical ‘AND’) start and stop features are classified (through the CNN model) as non-wear time, the candidate non-wear
#'   episode can be considered true non-wear time. If tstart is at t = 0, or tend is at the end of the acceleration data—meaning that
#'   those candidate non-wear episodes do not have a preceding or following window to extract features from—classify the start or stop
#'   as non-wear time by default.
#' @param df activity data, usually output from [read.gt3x::read.gt3x()],
#' and then imputed, or a data.frame of time/X/Y/Z
#' @param sample_rate sample rate (integer) of the sampling frequency in Hertz from the header
#' @param verbose print diagnostic messages
#' @param std_threshold standard deviation threshold in g
#' @param distance_in_min merge distance to group two nearby candidate nonwear episodes
#' @param episode_window_sec define window length to create input features
#' for the CNN model
#' @param edge default classification when an episode does not have a
#' starting or stop feature window (happens at t=0 or at the end of the data)
#' @param start_stop_label_decision logical operator to see if both
#' sides need to be classified as non-wear time (AND) or just a single side (OR)
#' @param outdir output directory to download the CNN model, passed to
#' \code{\link{download_cnn_model}}
#' @param min_segment_length minimum length of the segment (in minutes) to be candidate for
#' non-wear time, coerced to integer
#' @param sliding_window  sliding window (in minutes) that will go over
#' the acceleration data to find candidate non-wear segments, coerced to integer
#' @param model_path path to the model `h5` file, overrides `outdir` and
#' must match corresponding `episode_window_sec`.
#'
#' @return A list of data
#' @export
#'
#' @examples
#'
#' if (isTRUE(wt_packages_installed())) {
#'   path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#'                      package = "weartime")
#'   df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
#'                             imputeZeroes = TRUE,
#'                             verbose = TRUE)
#'   model_path = system.file("extdata", "cnn_v2_7.h5", package = "weartime")
#'   out = wt_cnn(df, outdir = tempdir(), model_path = model_path)
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
#' out = wt_cnn(df, outdir = tempdir())
#'
#' path = file.path(tempdir(), "AI12_NEO1F09120034_2017-09-25.gt3x.gz")
#' if (!file.exists(path)) {
#'   curl::curl_download(
#'     "https://ndownloader.figshare.com/files/21855675",
#'     destfile = path,
#'     quiet = FALSE)
#' }
#' df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
#'                            imputeZeroes = TRUE,
#'                            verbose = TRUE)
#' out = wt_cnn(df, outdir = tempdir())
#' }
#'
wt_cnn = function(
    df,
    sample_rate = NULL,
    std_threshold = 0.004,
    distance_in_min = 5L,
    episode_window_sec = 7L,
    edge = TRUE,
    start_stop_label_decision = c("and", "or"),
    min_segment_length = 1L,
    sliding_window = 1L,
    verbose = TRUE,
    model_path = NULL,
    outdir = NULL
) {

  check_py_packages()

  df = standardize_data(df, subset = TRUE)
  sample_rate = get_sample_rate(df, sample_rate, verbose)
  stopifnot(!is.null(sample_rate))
  times = df$time
  df$time = NULL
  if (verbose) {
    message("Converting to Numpy Array")
  }

  df = as.matrix(df)
  df = reticulate::np_array(df)

  start_stop_label_decision = match.arg(
    start_stop_label_decision)

  if (is.null(model_path)) {
    cnn_model_file = download_cnn_model(
      episode_window_sec = episode_window_sec,
      outdir = outdir,
      verbose = verbose > 1)
  } else {
    cnn_model_file = model_path
  }

  import_path = system.file(
    "extdata", "cnn",
    package = "weartime")
  wear = reticulate::import_from_path(
    "functions", path =  import_path,
    convert = TRUE)

  if (verbose) {
    message("Running Model")
  }

  out = wear$raw_non_wear_functions$cnn_nw_algorithm(
    raw_acc = df,
    hz = as.integer(sample_rate),
    cnn_model_file = cnn_model_file,
    std_threshold = std_threshold,
    distance_in_min = as.integer(distance_in_min),
    episode_window_sec = as.integer(episode_window_sec),
    edge_true_or_false = edge,
    start_stop_label_decision = start_stop_label_decision,
    nwt_encoding = 0L,
    wt_encoding = 1L,
    min_segment_length = as.integer(min_segment_length),
    sliding_window = as.integer(sliding_window),
    verbose = as.logical(verbose)
  )

  if (verbose) {
    message("Creating Output")
  }

  names(out) = c("nw_vector", "nw_data")
  stopifnot(ncol(out$nw_vector) ==1)
  stopifnot(length(out$nw_vector) == nrow(df))

  rm(df);
  out$nw_vector = c(out$nw_vector)
  if (!is.null(times)) {
    times_100 = seq(times[1],
                    lubridate::ceiling_date(times[length(times)], "1 second"),
                    by = lubridate::as.period(1/100, unit = "second"))
    times_100 = times_100[seq(length(out$nw_vector))]
    out = tibble::tibble(time = times_100, wear = out$nw_vector > 0)
  } else {
    out = tibble::tibble(wear = out$nw_vector > 0)
  }
  return(out)
}

#' Resample Accelerometer Data
#'
#' @param df activity data, usually output from [read.gt3x::read.gt3x()],
#' and then imputed, or a data.frame of time/X/Y/Z
#' @param sample_rate sample rate (integer) of the Hertz from the header
#' @param to_hz sample rate (integer) to resample to
#' @param verbose print diagnostic messages
#'
#' @return A \code{tibble} of resampled data
#' @export
#'
#' @examples
#'
#' reticulate::py_config()
#' if (isTRUE(wt_packages_installed())) {
#'   path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#'                      package = "weartime")
#'   df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
#'                             imputeZeroes = TRUE,
#'                             verbose = TRUE)
#'   res = resample_acc(df)
#' }
resample_acc = function(
    df,
    sample_rate = NULL,
    to_hz = 100L,
    verbose = TRUE
) {

  check_py_packages()

  df = standardize_data(df)
  sample_rate = get_sample_rate(df = df, sample_rate, verbose)
  stopifnot(!is.null(sample_rate))
  if (sample_rate == to_hz) {
    warning("Sample rate is identical to to_hz")
  }


  at = attributes(df)
  times = df$time
  df$time = NULL
  if (verbose) {
    message("Converting to Numpy Array")
  }
  cn = colnames(df)
  df = as.matrix(df)
  df = reticulate::np_array(df)


  import_path = system.file(
    "extdata", "cnn",
    package = "weartime")
  wear = reticulate::import_from_path(
    "functions", path =  import_path,
    convert = TRUE)

  out = wear$signal_processing_functions$resample_acceleration(
    data = df,
    from_hz = as.integer(sample_rate),
    to_hz = as.integer(to_hz),
    verbose = verbose)

  if (verbose) {
    message("Creating Output")
  }
  colnames(out) = cn
  out = tibble::as_tibble(out)
  rm(df)

  if (!is.null(times)) {
    times_100 = seq(times[1],
                    lubridate::ceiling_date(times[length(times)], "1 second"),
                    by = lubridate::as.period(1/100, unit = "second"))
    times_100 = times_100[seq(nrow(out))]
    out$time = times_100
    out = out[, c("time", cn)]
  }
  at_names = setdiff(names(at), names(attributes(out)))
  at$sample_rate = to_hz
  for (iattr in at_names) {
    attr(out, iattr) = at[[iattr]]
  }

  return(out)
}
