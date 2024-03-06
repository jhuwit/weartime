path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
                   package = "weartime")

testthat::test_that("Run CNN on TAS", {
  testthat::skip_if_not_installed("read.gt3x")
  df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
                            imputeZeroes = TRUE,
                            verbose = FALSE)
  out = wt_cnn(df, outdir = tempdir())
  testthat::expect_true(all(out$wear))
})

testthat::test_that("Resample TAS", {
  testthat::skip_if_not_installed("read.gt3x")
  df = read.gt3x::read.gt3x(path, asDataFrame = TRUE,
                            imputeZeroes = TRUE,
                            verbose = FALSE)
  out = resample_acc(df)
  testthat::expect_true(nrow(out) == 240500)
})
