% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wt_cnn.R
\name{resample_acc}
\alias{resample_acc}
\title{Resample Accelerometer Data}
\usage{
resample_acc(df, sample_rate = NULL, to_hz = 100L, verbose = TRUE)
}
\arguments{
\item{df}{activity data, usually output from \code{\link[read.gt3x:read.gt3x]{read.gt3x::read.gt3x()}},
and then imputed, or a data.frame of time/X/Y/Z}

\item{sample_rate}{sample rate (integer) of the Hertz from the header}

\item{to_hz}{sample rate (integer) to resample to}

\item{verbose}{print diagnostic messages}
}
\value{
A \code{tibble} of resampled data
}
\description{
Resample Accelerometer Data
}
\examples{

reticulate::py_config()
if (isTRUE(wt_packages_installed())) {
  path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
                     package = "weartime")
  df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
                            imputeZeroes = TRUE,
                            verbose = TRUE)
  res = resample_acc(df)
}
}
