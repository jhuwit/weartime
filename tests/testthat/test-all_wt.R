testthat::context("Trying all available WT models")
path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
                   package = "weartime")


testthat::test_that("Quick Sample rate test", {
  testthat::skip_if_not_installed("read.gt3x")
  df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
                            imputeZeroes = TRUE,
                            verbose = FALSE)
  x = df
  attr(x, "sample_rate") = NULL
  sr = weartime:::get_sample_rate(x)
  testthat::expect_equal(sr, 100L)
  x$time = NULL
  testthat::expect_error({
    weartime:::get_sample_rate(x)
    })
  rm(x)
})


testthat::test_that("Downloading the model", {
  download_cnn_model(outdir = tempdir())
  testthat::expect_true(
    file.exists(download_cnn_model(outdir = tempdir()))
  )
})

testthat::test_that("Trying the methods", {
  testthat::skip_if_not_installed("read.gt3x")
  df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
                            imputeZeroes = TRUE,
                            verbose = FALSE)
  methods = list(
    function(...) wt_cnn(..., outdir = tempdir()),
    wt_baseline,
    wt_hees_2011,
    wt_hees_2013,
    wt_hees_optimized,
    wt_vmu
  )
  results = lapply(methods, function(m) {
    m(df)
  })
  wears = sapply(results, function(r) all(r$wear))
  testthat::expect_true(all(wears))

  df$time = NULL
  results = lapply(methods, function(m) {
    m(df)
  })
  wears = sapply(results, function(r) all(r$wear))
  testthat::expect_true(all(wears))
})




