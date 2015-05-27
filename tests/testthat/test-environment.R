context("Environment")

test_that("load_packages", {
  expect_that(load_packages(NULL), not(throws_error()))
  expect_that(load_packages(character(0)), not(throws_error()))
  expect_that(load_packages(c("testthat", "utils")), is_null())
})

test_that("load_source_files", {
  expect_that(load_source_files(NULL), not(throws_error()))
  expect_that(load_source_files(character(0)), not(throws_error()))
  env <- new.env(parent=.GlobalEnv)
  load_source_files("src_a.R", env)
  expect_that(ls(env), equals(c("f", "g")))
  expect_that(env$f(1), equals(1))
  expect_that(attr(env, "source_files"), equals(hash_files("src_a.R")))

  env <- new.env(parent=.GlobalEnv)
  load_source_files(c("src_a.R", "src_b.R"), env)
  expect_that(ls(env), equals(c("f", "g")))
  expect_that(env$f(1), equals(2))
  expect_that(attr(env, "source_files"),
              equals(hash_files(c("src_a.R", "src_b.R"))))

  env <- new.env(parent=.GlobalEnv)
  load_source_files(c("src_b.R", "src_a.R"), env)
  expect_that(ls(env), equals(c("f", "g")))
  expect_that(env$f(1), equals(1))
  expect_that(attr(env, "source_files"),
              equals(hash_files(c("src_b.R", "src_a.R"))))
})

test_that("magrittr", {
  skip_on_cran()
  skip("need to fix this...")
  expect_that(load_source_files("src_magrittr.R"),
              throws_error("%>%", fixed=TRUE))

  if ("magrittr" %in% .packages(TRUE)) {
    on.exit(unloadNamespace("magrittr"))
    env <- new.env(parent=.GlobalEnv)
    load_source_files("src_magrittr.R", env, packages="magrittr")
    expect_that(ls(env), testthat::equals("s"))
    expect_that("magrittr" %in% .packages(), is_true())
    unloadNamespace("magrittr")
    expect_that("magrittr" %in% .packages(), is_false())
  }
})

test_that("current / reload", {
  str <- 'f <- function(x) x'
  filename <- "tmp.R"
  writeLines(str, filename)
  on.exit(file.remove(filename))
  env <- load_source_files(filename, new.env())

  expect_that(environment_current(env), is_true())
  expect_that(source_files_current(attr(env, "source_files")),
              is_true())
  expect_that(reload_source_files(env), is_identical_to(env))

  writeLines(c(str, ""), filename)

  expect_that(environment_current(env), is_false())
  expect_that(source_files_current(attr(env, "source_files")),
              is_false())

  env2 <- reload_source_files(env)
  expect_that(reload_source_files(env), not(is_identical_to(env)))

  expect_that(environment_current(env2), is_true())
  expect_that(source_files_current(attr(env2, "source_files")),
              is_true())
  expect_that(reload_source_files(env2), is_identical_to(env2))
})

test_that("current / reload_inplace", {
  filename <- "tmp.R"
  writeLines('f <- function(x) x', filename)
  on.exit(file.remove(filename))
  env <- load_source_files(filename, new.env())

  expect_that(environment_current(env), is_true())
  expect_that(env$f(1), equals(1))
  expect_that(reload_source_files_inplace(env), is_null())
  expect_that(env$f(1), equals(1))

  writeLines('f <- function(x) 2 * x', filename)
  expect_that(environment_current(env), is_false())
  expect_that(reload_source_files_inplace(env), is_null())
  expect_that(env$f(1), equals(2))
  expect_that(reload_source_files_inplace(env), is_null())
  expect_that(env$f(1), equals(2))
})
