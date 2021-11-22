# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Format Objects
#'
#' Format an object for printing, mostly used to print numeric data in the same
#' way SPSS.  This is mainly for internal use in \code{\link{print}} methods.
#'
#' @param x  an \R object, typically numeric.  Currently methods are
#' implemented for vectors, matrices and data frames.  The default method calls
#' \code{\link{as.character}}.
#' @param \dots  additional arguments passed down to methods.
#'
#' @return A character vector or matrix containing the formatted object.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # note how numbers in the interval (-1, 1) are printed
#' # without the zero in front of the comma
#' formatSPSS(c(-1.5, -2/3, 2/3, 1.5))
#'
#' @keywords utilities
#'
#' @export

formatSPSS <- function(x, ...) UseMethod("formatSPSS")


#' @rdname formatSPSS
#' @export

formatSPSS.default <- function(x, ...) as.character(x)


#' @rdname formatSPSS
#' @export

formatSPSS.integer <- function(x, ...) {
  # define format for integers
  n <- length(x)
  fmt <- rep.int("%d", n)
  # use empty string for NA
  fmt <- ifelse(is.finite(x), fmt, "")
  # convert integers to strings
  sprintf(fmt, x)
}


#' @rdname formatSPSS
#'
#' @param digits  an integer giving the number of digits after the comma to
#' display.
#'
#' @export

formatSPSS.numeric <- function(x, digits = 3, ...) {
  # define format with specified number of digits
  n <- length(x)
  digits <- rep_len(digits, n)
  fmt <- paste0("%.", digits, "f")
  # use empty string for NA
  fmt <- ifelse(is.finite(x), fmt, "")
  # convert numbers to strings
  formatted <- sprintf(fmt, x)
  # replace leading zeros
  formatted <- gsub("^0.", ".", formatted)    # positive numbers
  formatted <- gsub("^-0.", "-.", formatted)  # negative numbers
  # return
  formatted
}


#' @rdname formatSPSS
#' @export

formatSPSS.matrix <- function(x, ...) {
  # format as vector and add original attributes
  formatted <- formatSPSS(as.vector(x), ...)
  attributes(formatted) <- attributes(x)
  # return formatted matrix
  formatted
}


#' @rdname formatSPSS
#' @export

formatSPSS.data.frame <- function(x, ...) {
  # format each variable
  formatted <- lapply(x, formatSPSS, ...)
  formatted <- do.call(cbind, formatted)
  # add row names
  rownames(formatted) <- row.names(x)
  formatted
}
