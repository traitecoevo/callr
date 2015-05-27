context("run_system")

test_that("basic", {
  ## Just to pick something that will be installed for sure:
  R <- file.path(R.home("bin"), "R")

  dquote <- function(x) sprintf('"%s"', x)
  output <- run_system(R, c("-e", dquote("message('hello')")))
  expect_that(any(grepl("hello", output)), is_true())

  expect_that(run_system(R, c("-e", dquote("stop('myerror')"))),
              throws_error("Error: myerror"))
  expect_that(run_system(R, c("-e", dquote("stop('myerror')"))),
              throws_error("had status"))
})