#'Print a \code{dmodel} object
#'
#'@rdname print
#'@param x Object of class \code{dmodel}
#'@param ... Other parameters passed down to \code{print.default()}
#'@return \code{NULL}
#'@method print dmodel
#'@export
print.dmodel <- function(x, ...) {
  cat(
    "\nCall:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n",
    sep = ""
  )
  cat("\nObject class      ", class(x))
  cat("\nName              ", x$name)
  cat("\nCreated           ", as.character(x$date))
  cat("\nNumber of items   ", length(x$items))
  cat("\nNumber of persons ", nrow(x$fit$X))
  cat("\nNumber of equates ", length(x$fit$equate))
}
