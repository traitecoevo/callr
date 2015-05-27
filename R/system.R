##' The inverse of \code{\link{callr}}; this makes it easy to call a
##' system command from R and have it behave.
##'
##' This function uses \code{system2} to call a system command fairly
##' portably.  What it adds is a particular way of dealing with
##' errors.  \code{run_system} runs the command \code{command} with
##' arguments \code{args} (and with optionally set environment
##' variables \code{env}) and hides \emph{all} produced output to
##' stdout and stderr.  If the command fails (currently any nonzero
##' exit code is counted as a failure) then \code{run_system} will
##' throw an R error giving
##' \itemize{
##' \item{the full string of the command run}
##' \item{the exit code of the command}
##' \item{any \code{errmsg} attribute that might have been returned}
##' \item{all output that the program produced to either stdout and
##' stderr}
##' }
##'
##' This means that a successful invocation of a program produces no
##' output while the unsuccessful invocation throws an error and
##' prints all information to the screen (though this is delayed until
##' failure happens).
##'
##' \code{run_system} also returns the contents of both stderr and
##' stdout \emph{invisibly} so that it can be inspected if needed.
##' @title Run a system command, stopping on error
##' @param command The system command to be invoked, as a character
##' string.  \code{\link{Sys.which}} is useful here.
##' @param args A character vector of arguments to \code{command}
##' @param env A character vector of name=value pairs to be set as
##' environment variables (see \code{\link{system2}}).
##' @export
run_system <- function(command, args, env=character()) {
  res <- suppressWarnings(system2(command, args,
                                  env=env, stdout=TRUE, stderr=TRUE))
  ok <- attr(res, "status")
  if (!is.null(ok) && ok != 0) {
    cmd <- paste(c(env, shQuote(command), args), collapse = " ")

    msg <- sprintf("Running command:\n  %s\nhad status %d", cmd, ok)
    errmsg <- attr(cmd, "errmsg")
    if (!is.null(errmsg)) {
      msg <- c(msg, sprintf("%s\nerrmsg: %s", errmsg))
    }

    sep <- paste(rep("-", getOption("width")), collapse="")
    msg <- c(msg, "Program output:", sep, res, sep)
    stop(paste(msg, collapse="\n"))
  }
  invisible(res)
}
