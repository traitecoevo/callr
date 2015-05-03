context("Install packages")

## This will get fleshed out over time, but doing this without
## creating a horrible pile of dependencies is tricky.  I notice that
## devtools doesn't actually test installation.
##
## Update: this is crazy hard to test right.

test_that("check extra", {
  dat <-
    list(sowsear=list(source="github", repo="richfitz/sowsear"))

  cmp <- dat
  cmp$sowsear$call <- quote(devtools::install_github("richfitz/sowsear"))
  cmp$sowsear$name <- "sowsear"

  expect_that(check_package_sources(dat), equals(cmp))
})

test_that("install_packages_instructions", {
  package_sources <-
    list(sowsear=list(source="github", repo="richfitz/sowsear"))

  cmp_devtools_cran <- 'install.packages("devtools")'
  expect_that(install_packages_instructions("devtools", package_sources, FALSE),
              equals(cmp_devtools_cran))
  expect_that(install_packages_instructions("sowsear", package_sources, FALSE),
              equals(c(cmp_devtools_cran,
                       'devtools::install_github("richfitz/sowsear")')))

  ## Here's a weird case that allows bootstrapping of devtools:
  package_sources <- list(devtools=list(source="github",
                            repo="hadley/devtools"))
  expect_that(install_packages_instructions("devtools", package_sources, FALSE),
              equals(c(cmp_devtools_cran,
                       'devtools::install_github("hadley/devtools")')))

  ## Pick a package that does not exist:
  package_sources <- list(notreal=list(source="github", repo="notreal/notreal"))
  expect_that(install_packages_instructions("notreal", package_sources),
              equals('devtools::install_github("notreal/notreal")'))
})

## This test is going to be too slow to do most of the time.  So
## probably best to
test_that("install_packages (for reals)", {
  skip_unless_internet()
  skip_unless_set("TEST_INSTALL_PACKAGES")
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }

  ## Then try actually running this:
  package_sources <-
    list(sowsear=list(source="github", repo="richfitz/sowsear"))
  expect_that(install_packages_instructions("sowsear", package_sources),
              equals('devtools::install_github("richfitz/sowsear")'))
  res <- install_packages("sowsear", package_sources)
  expect_that(res, equals("sowsear"))
  expect_that("sowsear" %in% .packages(TRUE), is_true())

  res <- install_packages("sowsear", package_sources=package_sources)
  expect_that(res, equals(character(0)))
})
