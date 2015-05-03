skip_unless_set <- function(name) {
  if (identical(Sys.getenv(name), "true")) {
    return()
  }
  skip("Skipping install package tests")
}

skip_unless_internet <- function() {
  if (has_internet()) {
    return()
  }
  skip("No internet :(")
}

has_internet <- function() {
  !is.null(suppressWarnings(nsl("www.google.com")))
}
