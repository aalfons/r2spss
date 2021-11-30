# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Format Objects
#'
#' Format an object for printing, mostly used to print numeric data in the same
#' way as SPSS.  This is mainly for internal use in \code{\link{print}} methods.
#'
#' @param object  an \R object.  Currently methods are implemented
#' for vectors, matrices, and data frames.  The default method calls
#' \code{\link{as.character}}.
#' @param \dots  additional arguments passed down to methods.
#'
#' @return A character vector, matrix, or data frame containing the formatted
#' object.
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

formatSPSS <- function(object, ...) UseMethod("formatSPSS")


#' @rdname formatSPSS
#' @export

formatSPSS.default <- function(object, ...) as.character(object)


#' @rdname formatSPSS
#' @export

formatSPSS.integer <- function(object, ...) {
  # define format for integers
  n <- length(object)
  fmt <- rep.int("%d", n)
  # use empty string for NA
  fmt <- ifelse(is.finite(object), fmt, "")
  # convert integers to strings
  sprintf(fmt, object)
}


#' @rdname formatSPSS
#'
#' @param digits  an integer giving the number of digits after the comma to
#' display.
#'
#' @export

# formatSPSS.numeric <- function(object, digits = 3, pValue = FALSE,
#                                checkInt = FALSE,
#                                # tol = .Machine$double.eps^0.5,
#                                ...) {
#   # initializations
#   pValue <- isTRUE(pValue)
#   checkInt <- isTRUE(checkInt)
#   # -----
#   # the check below is too restrictive, for example the chi-squared
#   # goodness-of-fit test has the degrees of freedom and the p-value
#   # in the same column
#   # -----
#   # if (pValue && checkInt) {
#   #   stop("'pValue' and 'checkInt' may not both be TRUE")
#   # }
#   # -----
#   # define format with specified number of digits
#   n <- length(object)
#   digits <- rep_len(digits, n)
#   fmt <- paste0("%.", digits, "f")
#   # use empty string for NA
#   finite <- is.finite(object)
#   fmt <- ifelse(finite, fmt, "")
#   # if requested check for integers and change their format accordingly
#   if (checkInt) {
#     # -----
#     # Argument 'checkInt' is used when degrees of freedom are put in the same
#     # column as other numeric information, or when the column containing the
#     # degrees of freedom uses an approximation in one of the rows.  So it
#     # shouldn't be necessary to work with tolerances, which could mess things
#     # up, e.g., when a p-value in the same column is really close to 0 or 1.
#     # -----
#     # isInt <- finite & abs(object - as.integer(object)) < tol
#     # -----
#     isInt <- finite & (object == as.integer(object))
#     # -----
#     fmt <- ifelse(isInt, "%.0f", fmt)
#   } else isInt <- rep.int(FALSE, n)
#   # convert numbers to strings
#   formatted <- sprintf(fmt, object)
#   # if requested format p-value
#   if (pValue) {
#     zeros <- rep.int(0, n)
#     below <- finite & !isInt & (formatted == sprintf(fmt, zeros))
#     # formatted[below] <- gsub("^<0.", "$<$.", paste0("<", 10^(-digits[below])))
#     formatted[below] <- gsub("^<0.", "<.", paste0("<", 10^(-digits[below])))
#   }
#   # replace leading zeros
#   formatted <- gsub("^0.", ".", formatted)    # positive numbers
#   formatted <- gsub("^-0.", "-.", formatted)  # negative numbers
#   # return
#   formatted
# }

formatSPSS.numeric <- function(object, digits = 3, pValue = FALSE,
                               checkInt = FALSE, ...) {
  # initializations
  n <- length(object)
  digits <- rep_len(digits, n)
  pValue <- rep_len(sapply(pValue, isTRUE), n)
  checkInt <- rep_len(sapply(checkInt, isTRUE), n)
  # define format with specified number of digits
  fmt <- paste0("%.", digits, "f")
  # use empty string for NA
  finite <- is.finite(object)
  fmt <- ifelse(finite, fmt, "")
  # if requested check for integers and change their format accordingly
  isInt <- finite & checkInt & (object == as.integer(object))
  fmt <- ifelse(isInt, "%.0f", fmt)
  # convert numbers to strings
  formatted <- sprintf(fmt, object)
  # if requested format p-value
  zeros <- rep.int(0, n)
  below <- finite & !isInt & pValue & (formatted == sprintf(fmt, zeros))
  formatted[below] <- gsub("^<0.", "<.", paste0("<", 10^(-digits[below])))
  # replace leading zeros
  formatted <- gsub("^0.", ".", formatted)    # positive numbers
  formatted <- gsub("^-0.", "-.", formatted)  # negative numbers
  # return
  formatted
}


#' @rdname formatSPSS
#' @export

formatSPSS.matrix <- function(object, digits = 3, pValue = FALSE,
                              checkInt = FALSE, ...) {
  # initializations
  d <- dim(object)
  pValue <- rep_len(pValue, d[2])
  checkInt <- rep_len(checkInt, d[2])
  # format each column
  formatted <- vapply(seq_len(d[2]), function(j) {
    formatSPSS(object[, j], digits = digits, pValue = pValue[j],
               checkInt = checkInt[j])
  }, character(d[1]))
  # add original attributes
  attributes(formatted) <- attributes(object)
  # return formatted matrix
  formatted
}


#' @rdname formatSPSS
#' @export

formatSPSS.data.frame <- function(object, digits = 3, pValue = FALSE,
                                  checkInt = FALSE, ...) {
  # initializations
  d <- dim(object)
  pValue <- rep_len(pValue, d[2])
  checkInt <- rep_len(checkInt, d[2])
  # format each column
  formatted <- mapply(function(v, p, c) {
    formatSPSS(v, digits = digits, pValue = p, checkInt = c)
  }, v = object, p = pValue, c = checkInt, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  # add original attributes
  attributes(formatted) <- attributes(object)
  # return formatted matrix
  formatted
}
