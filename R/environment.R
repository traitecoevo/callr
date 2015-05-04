##' Load a set of packages, possibly installing first.
##'
##' @title Load a set of packages
##' @param packages Character vector with a set of packages to load
##' @param package_sources Information on where to install additional
##' package from (see \code{\link{install_packages}}
##' @param quiet Load packages quietly?
##' @param install_missing Attempt to install missing packages?
##' @export
load_packages <- function(packages, package_sources=NULL, quiet=TRUE,
                          install_missing=FALSE) {
  if (install_missing) {
    install_packages(packages, package_sources, missing_only=TRUE)
  }
  for (p in packages) {
    if (quiet) {
      suppressMessages(library(p, character.only=TRUE, quietly=TRUE))
    } else {
      library(p, character.only=TRUE)
    }
  }
}

##' Load a set of source files.  The function \code{load_source_files}
##' is roughly equivalent to running a for loop over a set of source
##' files, except that it preloads some packages, keeps a hash of the
##' source files that were generated, has slightly better error
##' messaging and is focussed on creating environments.
##'
##' Note that the if sourced files source other files then the
##' environment will not easily be recreatable (this is due to the
##' difficulty of redirecting where source is pointing to in
##' subsequent calls).
##' @title Load a set of source files
##' @param source_files Characte vector with a set of source files to
##' load.  Paths should be relative to the current working directory,
##' though absolute paths are allowed.
##' @param envir Environment to source into.  By default, sources into
##' the global environment (as does \code{\link{source}}, but in
##' contrast to \code{\link{sys.source}}).
##' @param packages Packages to load before sourcing the files.  This
##' is perhaps useful for code that uses the magrittr pipe to define
##' functions, or for source files that generate objects as opposed to
##' functions.
##' @param ... Additional arguments to \code{\link{sys.source}} (i.e.,
##' \code{chdir} and \code{keep.source}).
##' @export
load_source_files <- function(source_files, envir=.GlobalEnv,
                              packages=character(0), ...) {
  load_packages(packages)
  for (file in source_files) {
    do_source(file, envir, ...)
  }
  attr(envir, "source_files") <- hash_files(source_files)
  invisible(envir)
}

source_files_current <- function(source_files) {
  identical(tools::md5sum(source_files), source_files)
}

##' Test if environments are current and reload out-of-date
##' environments.
##'
##' If source_files are reloaded, a \emph{new} environment is created; this
##' environment will have the same parent as \code{envir}.  It would
##' be straightforward, but not implemented, to delete all the objects
##' from \code{envir} and re-source into that environment though.
##'
##' Test if source_files are current, based on their md5 signature.
##' Nothing clever is done here; missing files, changed files, etc all
##' return \code{FALSE}.
##' @title Test if source files are current
##' @param envir Environment to load
##' @param source_files Named character vector where names correspond
##' to filenames and the contents correspond to md5sums returned by
##' \code{link{md5sum}}.  This is the format in the attribute
##' \code{source_files} from \code{\link{load_source_files}} when run with
##' \code{collect_hash=TRUE}.
##' @param ... Additional arguments to \code{\link{load_source_files}}.
##' @export
reload_source_files <- function(envir,
                                source_files=attr(envir, "source_files"),
                                ...) {
  if (source_files_current(source_files)) {
    envir
  } else {
    load_source_files(names(source_files),
                      new.env(parent=parent.env(envir)), ...)
  }
}

## This runs source but with better error messages when sourcing
## multiple files.
do_source <- function(file, envir, ...) {
  catch_source <- function(e) {
    stop(sprintf("while sourcing %s:\n%s", file, e$message),
         call.=FALSE)
  }
  tryCatch(sys.source(file, envir, ...),
           error=catch_source)
}

## Utilities to keep this all self-contained in case I move it.
hash_files <- function(x) {
  if (length(x) == 0L) {
    structure(character(0), names=character(0))
  } else {
    tools::md5sum(x)
  }
}

## Not sure about keeping this though it is nice.
relative_path <- function(path, wd=getwd(), normalize=FALSE) {
  if (normalize) {
    path <- normalizePath(path)
  }
  path <- gsub("/+", "/", path)
  wd <- sub("/$", "", gsub("/+", "/", wd))
  n <- nchar(wd)
  if (substr(path, 1, n) == wd) {
    path <- sub("^/", "", substr(path, nchar(wd) + 1L, nchar(path)))
  }
  path
}
