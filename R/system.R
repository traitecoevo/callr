##' The inverse of \code{\link{callr}}; this makes it easy to call a
##' system command from R and have it behave.
##'
##' This function uses \code{system2} to call a system command fairly
##' portably.  What it adds is a particular way of dealing with
##' errors.  \code{call_system} runs the command \code{command} with
##' arguments \code{args} (and with optionally set environment
##' variables \code{env}) and hides \emph{all} produced output to
##' stdout and stderr.  If the command fails (currently any nonzero
##' exit code is counted as a failure) then \code{call_system} will
##' throw an R error giving
##' \itemize{
##' \item the full string of the command run
##' \item the exit code of the command
##' \item any \code{errmsg} attribute that might have been returned
##' \item all output that the program produced to either stdout and
##' stderr
##' }
##'
##' This means that a successful invocation of a program produces no
##' output while the unsuccessful invocation throws an error and
##' prints all information to the screen (though this is delayed until
##' failure happens).
##'
##'
##' \code{call_system} also returns the contents of both stderr and
##' stdout \emph{invisibly} so that it can be inspected if needed.
##'
##' The function \code{run_system} does the same thing and will be
##' removed as soon as code that depends on it is out of use.
##'
##' @title Run a system command, stopping on error
##' @param command The system command to be invoked, as a character
##'   string.  \code{\link{Sys.which}} is useful here.
##' @param args A character vector of arguments to \code{command}
##' @param env A character vector of name=value pairs to be set as
##'   environment variables (see \code{\link{system2}}).
##' @param max_lines Maximum number of lines of program output to
##'   print with the error message.  We may prune further to get the
##'   error message under \code{getOption("warn.length")}, however.
##' @param p Fraction of the error message to show from the tail of
##'   the output if truncating on error (default is 20\% lines are
##'   head, 80\% is tail).
##' @param stdout,stderr Passed to \code{system2}.  Set one of these
##'   to \code{FALSE} to avoid capturing output from that stream.
##'   Setting both to \code{FALSE} is not recommended.
##' @param locate_command The \code{command} will be passed through
##'   \code{\link{Sys_which}} to find the full path, and will report a
##'   sensible error if the command can't be found.
##' @export
##'
call_system <- function(command, args, env=character(), max_lines=20,
                        p=0.8, stdout=TRUE, stderr=TRUE, locate_command=TRUE) {
  if (locate_command) {
    command <- Sys_which(command)
  }
  res <- suppressWarnings(system2(command, args,
                                  env=env, stdin="",
                                  stdout=stdout, stderr=stderr))
  ## TODO: need to better allow stdout/stderr to *not* be captured.
  ok <- attr(res, "status")
  if (!is.null(ok) && ok != 0) {
    max_nc <- getOption("warning.length")

    cmd <- paste(c(env, shQuote(command), args), collapse = " ")
    msg <- sprintf("Running command:\n  %s\nhad status %d", cmd, ok)
    errmsg <- attr(cmd, "errmsg")
    if (!is.null(errmsg)) {
      msg <- c(msg, sprintf("%s\nerrmsg: %s", errmsg))
    }
    sep <- paste(rep("-", getOption("width")), collapse="")

    ## Truncate message:
    if (length(res) > max_lines) {
      n <- ceiling(max_lines * p)
      res <- c(head(res, ceiling(max_lines - n)),
               sprintf("[[... %d lines dropped ...]]", length(res) - max_lines),
               tail(res, ceiling(n)))
    }

    ## compute the number of characters so far, including three new lines:
    nc <- (nchar(msg) + nchar(sep) * 2) + 3
    i <- max(1, which(cumsum(rev(nchar(res) + 1L)) < (max_nc - nc)))
    res <- res[(length(res) - i + 1L):length(res)]
    msg <- c(msg, "Program output:", sep, res, sep)
    stop(paste(msg, collapse="\n"))
  }
  invisible(res)
}

##' Like \code{Sys.which} but throws error of command not found.
##' @title Find full path to executable
##' @param name Name or path of possible exectutable to look for
##' @export
##' @examples
##' try(Sys_which("R"))
##' try(Sys_which("not_installed_program"))
Sys_which <- function(name) {
  if (length(name) != 1L) {
    stop("'name' must be a scalar")
  }
  ret <- Sys.which(name)
  if (ret == "") {
    stop(sprintf("%s not found in $PATH", name))
  }
  ret
}
