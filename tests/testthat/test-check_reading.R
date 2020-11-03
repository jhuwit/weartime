path = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
                   package = "pygt3x")
res = pygt3x::py_read_gt3x(path)
df = pygt3x::impute_zeros(res$data, res$dates, res$header)

testthat::test_that("Run CNN on TAS", {
  out = wt_cnn(df, outdir = tempdir())
  testthat::expect_true(all(out$wear))
})

testthat::test_that("Resample TAS", {
  out = resample_acc(df)
  testthat::expect_true(nrow(out) == 240500)
})
