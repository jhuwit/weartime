#' Download CNN for Wear Time Estimation
#'
#' @param episode_window_sec define window length to create input features for the CNN model
#' @param outdir output directory for `h5` file
#'
#' @return A path to the `h5` file
#' @export
#'
#' @examples
#' download_cnn_model()
download_cnn_model = function(
  episode_window_sec = 7,
  outdir = NULL) {
  stopifnot(episode_window_sec %in% 2:10)
  episode_window_sec = episode_window_sec[1]
  url = "https://github.com/shaheen-syed/CNN-Non-Wear-Time-Algorithm/"
  path = sprintf("raw/master/cnn_models/cnn_v2_%d.h5", episode_window_sec)
  url = paste0(url, path)
  if (is.null(outdir)) {
    outdir = system.file("extdata", package = "weartime")
  }
  destfile = file.path(outdir, basename(path))
  curl::curl_download(url = url, destfile = destfile)
  return(destfile)
}
