##' Install packages, including from non-CRAN sources
##'
##' \code{package_sources} must be a named list; the list corresponds
##' to package names.  Each element is a list, most likely with
##' elements \code{source="github"} and \code{repo="username/repo"}.
##' @title Install packages
##' @param packages Vector of packages
##' @param package_sources List with details for installation of
##' non-CRAN sources.
##' @param missing_only Install only missing packages?
##' @export
install_packages <- function(packages, package_sources=NULL,
                             missing_only=TRUE) {
  dat <- install_packages_prepare(packages, package_sources, missing_only)
  install_packages_cran(dat$cran)
  install_packages_extra(dat$extras)
  invisible(c(dat$cran, names(dat$extras)))
}

install_packages_instructions <- function(packages,
                                          package_sources=NULL,
                                          missing_only=TRUE) {
  dat <- install_packages_prepare(packages, package_sources, missing_only)
  c(install_packages_instructions_cran(dat$cran),
    install_packages_instructions_extra(dat$extras))
}

## TODO: need to get the repos argument in here.
install_packages_prepare <- function(packages, package_sources=NULL,
                                     missing_only=TRUE) {
  if (missing_only) {
    packages <- missing_packages(packages)
  }
  if (length(packages) == 0L) {
    list(cran=character(0), extra=list())
  } else if (is.null(package_sources)) {
    list(cran=packages, extra=list())
  } else {
    package_sources <- check_package_sources(package_sources)
    extras <- package_sources[names(package_sources) %in% packages]
    from_cran <- setdiff(packages, names(extras))

    ## devtools bootstrap:
    if (length(extras) > 0L) {
      if (missing_only) {
        from_cran <- union(from_cran, missing_packages("devtools"))
      } else {
        from_cran <- c(from_cran, "devtools")
      }
    } else {
      extras <- list()
      from_cran <- packages
    }
    list(cran=from_cran, extras=extras)
  }
}

install_packages_cran <- function(packages) {
  if (length(packages) == 0L) {
    return(character(0))
  }
  ## This is super annoying but means that we'll hopefully get
  ## somewhere when CRAN is not set, avoiding the
  ##   trying to use CRAN without setting a mirror
  ## error.
  ##
  ## TODO: getting this "right" is really tricky: everything works
  ## great in interactive mode, but the R CMD check environment is
  ## different.  I suspect it's documented on one page of the PDF,
  ## but for the meantime this should do.
  r <- getOption("repos")
  r["CRAN"] <- "http://cran.rstudio.org"
  ## NOTE: warn=2 might not be the right idea, or possibly should be
  ## configurable, as sometimes "fail and keep going" is the right
  ## thing to do.
  oo <- options(repos=r, warn=2)
  on.exit(options(oo))

  install.packages(packages)
  packages
}

install_packages_instructions_cran <- function(packages) {
  if (length(packages) == 0L) {
    character(0)
  } else {
    sprintf("install.packages(%s)",
            paste(dquote(packages), collapse=", "))
  }
}

## We assume that the packages listed in package_sources don't have
## complicated dependencies and all come *after* the packages on
## CRAN.  So the CRAN packages are installed and *then* the packages
## here are installed.  It's not wonderful, but it will work.
##
## Packages that depend on github packages will fail if we go through
## sequentially.  But if we go through simultaneously we can't easily
## pass in extra arguments.  There's no way of winning here without a
## more comprehensive set of package infrastructure.  This is where
## packrat shines, but that's just too much here.
install_packages_extra <- function(dat) {
  if (length(dat) == 0L) {
    return(character(0))
  }

  for (i in dat) {
    e <- new.env(parent=.GlobalEnv)
    eval(i$call, e)
  }
  names(dat)
}

install_packages_instructions_extra <- function(dat) {
  vcapply(dat, function(x) deparse(x$call), USE.NAMES=FALSE)
}

missing_packages <- function(packages) {
  setdiff(packages, .packages(TRUE))
}

install_function <- function(src) {
  switch(src,
         github="install_github",
         bitbucket="install_bitbucket",
         url="install_url",
         git="install_git",
         stop("Invalid source ", src))
}

dquote <- function(x) {
  sprintf('"%s"', x)
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

match_value <- function(arg, choices, name=deparse(substitute(arg))) {
  if (length(arg) != 1L || !is.character(arg)) {
    stop("arg must be a scalar character")
  }
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(dQuote(choices), collapse=", ")))
  }
  arg
}

check_package_sources <- function(dat) {
  required <- list(github="repo",
                   bitbucket="repo",
                   url="url",
                   git="git_url")

  install_packages_extra_call <- function(package) {
    x <- dat[[package]]
    if (is.null(names(x)) || any(names(x) == "")) {
      stop(sprintf("All arguments of package '%s' must be named", package))
    }
    if (!("source" %in% names(x))) {
      stop(sprintf("'source' must be present in package: '%s'", package))
    }

    src <- match_value(x$source, names(required))
    req <- required[[src]]
    msg <- setdiff(req, names(x))
    if (length(msg) > 0L) {
      stop(sprintf("Required fields missing from %s: %s",
                   package, paste(msg, collapse=", ")))
    }

    ## Move first argument to the first position and unname it, to
    ## match canonical style:
    args <- c(unname(x[req]), x[setdiff(names(x), c("source", req))])

    fn_name <- parse(text=paste0("devtools::", install_function(x$source)))[[1]]
    x$call <- as.call(c(list(fn_name), args))
    x$name <- package
    x
  }

  if (length(dat) > 0L) {
    if (is.null(names(dat))) {
      stop("dat must be named")
    }
    for (i in names(dat)) {
      dat[[i]] <- install_packages_extra_call(i)
    }
  }
  dat
}
