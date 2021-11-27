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

formatSPSS.numeric <- function(x, digits = 3, pValue = FALSE, checkInt = FALSE,
                               tol = .Machine$double.eps^0.5, ...) {
  # initializations
  pValue <- isTRUE(pValue)
  checkInt <- isTRUE(checkInt)
  if (pValue && checkInt) {
    stop("'pValue' and 'checkInt' may not both be TRUE")
  }
  # define format with specified number of digits
  n <- length(x)
  digits <- rep_len(digits, n)
  fmt <- paste0("%.", digits, "f")
  # use empty string for NA
  finite <- is.finite(x)
  fmt <- ifelse(finite, fmt, "")
  # if requested check for integers and change their format accordingly
  if (checkInt) {
    isInt <- finite & abs(x - as.integer(x)) < tol
    fmt <- ifelse(isInt, "%.0f", fmt)
  }
  # convert numbers to strings
  formatted <- sprintf(fmt, x)
  # if requested format p-value
  if (pValue) {
    zeros <- rep.int(0, n)
    below <- finite & formatted == sprintf(fmt, zeros)
    formatted[below] <- gsub("^<0.", "$<$.", paste0("<", 10^(-digits[below])))
  }
  # replace leading zeros
  formatted <- gsub("^0.", ".", formatted)    # positive numbers
  formatted <- gsub("^-0.", "-.", formatted)  # negative numbers
  # return
  formatted
}


#' @rdname formatSPSS
#' @export

formatSPSS.matrix <- function(x, digits = 3, pValue = NULL, checkInt = FALSE,
                              tol = .Machine$double.eps^0.5, ...) {
  # initializations
  d <- dim(x)
  colNames <- colnames(x)
  if (is.null(pValue)) {
    if (is.null(colNames)) pValue <- rep.int(FALSE, d[2])
    else pValue <- grepl("Sig.", colNames, fixed = TRUE)
  } else pValue <- rep_len(pValue, d[2])
  checkInt <- rep_len(checkInt, d[2])
  # format each column and add original attributes
  formatted <- vapply(seq_len(d[2]), function(j) {
    formatSPSS(x[, j], digits = digits, pValue = pValue[j],
               checkInt = checkInt[j], tol = tol)
  }, character(d[1]))
  attributes(formatted) <- attributes(x)
  # return formatted matrix
  formatted
}


#' @rdname formatSPSS
#' @export

formatSPSS.data.frame <- function(x, digits = 3,
                                  pValue = NULL, checkInt = FALSE,
                                  tol = .Machine$double.eps^0.5, ...) {
  # initializations
  d <- dim(x)
  colNames <- names(x)
  if (is.null(pValue)) pValue <- grepl("Sig.", colNames, fixed = TRUE)
  else pValue <- rep_len(pValue, d[2])
  checkInt <- rep_len(checkInt, d[2])
  # format each column
  formatted <- mapply(function(v, pv, ci) {
    formatSPSS(v, digits = digits, pValue = pv, checkInt = ci, tol = tol)
  }, v = x, pv = pValue, ci = checkInt, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  formatted <- do.call(cbind, formatted)
  # add row names
  rownames(formatted) <- row.names(x)
  formatted
}
