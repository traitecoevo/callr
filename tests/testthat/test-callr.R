context("callr")

file.remove(dir(".", pattern=glob2rx("test*.json*")))

test_that("Simple use", {
  dat <- list("function"="list",
              args=list(list(name="foo", value="bar")))
  json <- jsonlite::toJSON(dat, auto_unbox=TRUE)
  writeLines(json, "test.json")

  res <- callr("test.json", "test_out.json")
  expect_that(res, equals(list(foo="bar")))
  json_out <- jsonlite::fromJSON("test_out.json", FALSE)
  expect_that(json_out$value, equals(res))
  expect_that(json_out[c("function", "args")],
              equals(dat))
})

test_that("Backup", {
  dat <- list("function"="list",
              args=list(list(name="foo", value="bar")))
  json <- jsonlite::toJSON(dat, auto_unbox=TRUE)
  writeLines(json, "test.json")
  str <- readLines("test.json")

  res <- callr("test.json", backup=TRUE)
  expect_that(file.exists("test.json.1"), is_true())
  expect_that(readLines("test.json.1"), equals(str))
})

test_that("multiple args", {
  dat <- list("function"="base::list",
              args=list(
                list(name="foo", value=1:10),
                list(name="baz", value="another")))
  json <- jsonlite::toJSON(dat, auto_unbox=TRUE)
  writeLines(json, "test.json")

  res <- callr("test.json")
  expect_that(res, equals(list(foo=1:10, baz="another")))
})

test_that("Unnamed args", {
  dat <- list("function"="base::list",
              args=list(
                list(value=1:10),
                list(name="baz", value="another")))
  json <- jsonlite::toJSON(dat, auto_unbox=TRUE)
  writeLines(json, "test.json")

  res <- callr("test.json")
  expect_that(res, equals(list(1:10, baz="another")))
})

test_that("sources", {
  dat <- list("function"="myfun",
              sources="functions.R",
              args=list(
                list(value=1:10)))
  json <- jsonlite::toJSON(dat, auto_unbox=TRUE)
  writeLines(json, "test.json")

  res <- callr("test.json")
  expect_that(res, equals(1:10))
})

test_that("extra fields", {
  dat <- list("function"="myfun",
              sources="functions.R",
              extra="another thing",
              args=list(
                list(value=1:10)))
  json <- jsonlite::toJSON(dat, auto_unbox=TRUE)
  writeLines(json, "test.json")

  expect_that(res <- callr("test.json"), not(gives_warning()))
  expect_that(res, equals(1:10))

  expect_that(res2 <- callr("test.json", strict=TRUE),
              gives_warning("Unknown fields"))
  expect_that(res2, equals(1:10))
})


test_that("options", {
  opts <- parse_opts(c("myfile"))
  expect_that(opts$filename, equals("myfile"))
  expect_that(opts$outfile,  is_null())
  expect_that(opts$ugly,     is_false())
  expect_that(opts$vectors,  is_false())
  expect_that(opts$backup,   is_false())
  expect_that(opts$strict,   is_false())

  opts <- parse_opts(c("myfile", "--ugly", "--backup", "--vectors", "--strict"))
  expect_that(opts$filename, equals("myfile"))
  expect_that(opts$outfile,  is_null())
  expect_that(opts$ugly,     is_true())
  expect_that(opts$vectors,  is_true())
  expect_that(opts$backup,   is_true())
  expect_that(opts$strict,   is_true())
})
