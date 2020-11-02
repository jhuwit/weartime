path = file.path(tempdir(), "AI12_NEO1F09120034_2017-09-25.gt3x.gz")
if (!file.exists(path)) {
  curl::curl_download(
    "https://ndownloader.figshare.com/files/21855675",
    destfile = path,
    quiet = FALSE)
}
testthat::test_that("Downloading CNN Model", {
  x = download_cnn_model(outdir = tempdir())
  testthat::expect_true(file.exists(x))
})


testthat::test_that("No index given", {
  res = pygt3x::py_read_gt3x(path)
  df = pygt3x::impute_zeros(res$data, res$dates, res$header)
  testthat::expect_equal(nrow(df), 18144000L)
  dtime = structure(c(1506333600, 1506938399.9666666),
                    class = c("POSIXct",
                              "POSIXt"),
                    tzone = "GMT")
  testthat::expect_equal(range(df$time), dtime, tolerance = 1e-5)
  out = wt_cnn(df, outdir = tempdir())

  testthat::expect_equal(sum(out), 53279758L)
  testthat::expect_equal(sum(!out), 7200242L)
})


