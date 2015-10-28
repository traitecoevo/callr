##' Call an R function via JSON
##' @title Call an R function via JSON
##' @param filename_in Filename for JSON with function to run
##' @param filename_out Filename to write the output to
##' @param backup Backup existing file when
##' @param prettify Produce pretty json output?
##' @param auto_unbox Turn length-1 vectors into scalars in json output?
##' @param strict Warn about additional fields in the json?
##' @export
callr <- function(filename_in, filename_out=NULL,
                  backup=FALSE, prettify=TRUE, auto_unbox=TRUE,
                  strict=FALSE) {
  if (is.null(filename_out)) {
    filename_out <- filename_in
  }
  dat <- read_callr_json(filename_in, strict)
  load_source_files(dat$sources, .GlobalEnv, packages=dat$packages)
  call <- json_call(dat)
  value <- eval(call, .GlobalEnv)
  dat$dat$value <- value
  json <- jsonlite::toJSON(dat$dat, auto_unbox=auto_unbox)
  if (prettify) {
    json <- jsonlite::prettify(json)
  }
  if (backup && filename_in == filename_out) {
    backup(filename_in)
  }
  writeLines(json, filename_out)
  invisible(value)
}

##' Installs the "callr" script in the directory \code{path}
##' @title Install callr script
##' @param path Directory to install the script
##' @export
install_callr <- function(path) {
  code <- c("#!/usr/bin/env Rscript", "library(methods)", "callr:::main()")
  dest <- file.path(path, "callr")
  writeLines(code, dest)
  Sys.chmod(dest, "0755")
}

main <- function(args=commandArgs(TRUE)) {
  opts <- parse_opts(args)
  callr(opts$filename,
        opts$outfile,
        backup     =  opts$backup,
        prettify   = !opts$ugly,
        auto_unbox = !opts$vectors)
}

##' @importFrom docopt docopt
parse_opts <- function(args) {
  oo <- options(warnPartialMatchArgs=FALSE)
  if (isTRUE(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }
  "Usage:
  callr [options] <filename> [<outfile>]

Options:
  -h --help      Show this screen.
  -b --backup    Make a backup of filename if outfile is same as filename?
  -u --ugly      Don't prettify json output
  -s --strict    Warn about extra fields
  -v --vectors   Don't auto unbox vectors to scalars" -> doc

  docopt::docopt(doc, args)
}

##' @importFrom jsonlite fromJSON
read_callr_json <- function(filename, strict) {
  ## This disables warning about the last line:
  dat <- jsonlite::fromJSON(readLines(filename, warn=FALSE),
                            simplifyVector=TRUE,
                            simplifyMatrix=FALSE,
                            simplifyDataFrame=FALSE)

  if (strict) {
    valid <- c("function", "args", "packages", "sources", "value")
    extra <- setdiff(names(dat), valid)
    if (length(extra) > 0L) {
      warning(sprintf("Unknown fields in %s: %s",
                      filename, paste(extra, collapse=", ")))
    }
  }

  list("function"=read_callr_function(dat[["function"]]),
       args=read_callr_args(dat[["args"]]),
       packages=read_callr_packages(dat[["packages"]]),
       sources=read_callr_sources(dat[["sources"]]),
       dat=dat)
}

read_callr_function <- function(fun) {
  if (is.null(fun)) {
    stop("Expected a function")
  }
  if (length(fun) != 1L) {
    stop("function must be scalar")
  }
  if (grepl("::", fun, fixed=TRUE)) {
    fun2 <- strsplit(fun, "::", fixed=TRUE)[[1]]
    if (length(fun2) != 2L) {
      stop("Not a namespace-qualified variable")
    }
    call("::", as.name(fun2[[1]]), as.name(fun2[[2]]))
  } else {
    as.name(fun)
  }
}

read_callr_args <- function(args) {
  if (!is.null(args)) {
    name <- lapply(args, "[[", "name")
    msg <- vapply(name, is.null, logical(1))
    if (all(msg)) {
      name <- NULL
    } else {
      name[msg] <- ""
      name <- as.character(name)
    }
    args <- setNames(lapply(args, "[[", "value"), name)
  }
  args
}

read_callr_packages <- function(packages) {
  if (!is.null(packages)) {
    if (!is.character(packages)) {
      stop("packages must be a character vector")
    }
  }
  packages
}

read_callr_sources <- function(sources) {
  if (!is.null(sources)) {
    if (!is.character(sources)) {
      stop("sources must be a character vector")
    }
    ok <- file.exists(sources)
    if (!all(ok)) {
      stop("Source files do not exist: ",
           paste(sources[!ok], collapse=", "))
    }
  }
  sources
}

backup <- function(filename, verbose=FALSE, move=FALSE) {
  if (file.exists(filename)) {
    pat <- sprintf("%s\\.([0-9]+)", basename(filename))
    found <- dir(dirname(filename), pattern=pat)
    if (length(found) > 0) {
      n <- max(as.integer(sub(pat, "\\1", found))) + 1
    } else {
      n <- 1
    }
    dest <- sprintf("%s.%d", filename, n)
    if (verbose) {
      action <- if (move) "Moving" else "Copying"
      message(sprintf("%s %s -> %s", action, filename, basename(dest)))
    }
    if (move) {
      file.rename(filename, dest)
    } else {
      file.copy(filename, dest)
    }
  }
}

json_call <- function(dat=read_callr_json(filename, FALSE), filename=NULL) {
  as.call(c(dat[["function"]], dat[["args"]]))
}

## Minimal version of this thing, until I can factor out the
## 'environment' code properly, probably into installr.
load_source_files <- function(source_files, envir=.GlobalEnv,
                              packages=character(0), ...) {
  do_source <- function(file, envir, ...) {
    catch_source <- function(e) {
      stop(sprintf("while sourcing %s:\n%s", file, e$message),
           call.=FALSE)
    }
    tryCatch(sys.source(file, envir, ...),
             error=catch_source)
  }
  for (p in packages) {
    suppressMessages(library(p, character.only=TRUE, quietly=TRUE))
  }
  for (file in source_files) {
    do_source(file, envir, ...)
  }
  invisible(envir)
}
