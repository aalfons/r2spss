# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Format Objects
#'
#' Format an object for printing, mostly used to print numeric data in the same
#' way as SPSS.  This is mainly for internal use in \code{\link{to_SPSS}} and
#' \code{\link{print}} methods.
#'
#' @param object  an \R object.  Currently methods are implemented
#' for vectors, matrices, and data frames.  The default method calls
#' \code{\link{as.character}}.
#' @param digits  an integer giving the number of digits after the comma to
#' display.
#' @param p_value  a logical indicating whether small positive values should be
#' indicated as below the threshold defined by \code{digits}, e.g.,
#' \code{"<.001"} if \code{digits = 3}.  This is used for formatting p-values
#' in LaTeX tables that mimic the look of SPSS.  For the \code{"numeric"}
#' method, a logical vector indicates the behavior for each element of
#' \code{object}.  For the \code{"matrix"} or \code{"data.frame"} methods, a
#' logical vector indicates the behavior for each column of \code{object}.
#' @param check_int  a logical indicating whether to check for integer values
#' and format them as such, e.g., to format the integer \code{2} as \code{"2"}
#' instead of \code{"2.000"} if \code{digits = 3}.  For the \code{"numeric"}
#' method, a logical vector indicates the behavior for each element of
#' \code{object}.  For the \code{"matrix"} or \code{"data.frame"} methods, a
#' logical vector indicates the behavior for each column of \code{object}.
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
#' format_SPSS(c(-1.5, -2/3, 2/3, 1.5))
#'
#' @keywords utilities
#'
#' @export

format_SPSS <- function(object, ...) UseMethod("format_SPSS")


#' @rdname format_SPSS
#' @export

format_SPSS.default <- function(object, ...) as.character(object)


#' @rdname format_SPSS
#' @export

format_SPSS.integer <- function(object, ...) {
  # define format for integers
  n <- length(object)
  fmt <- rep.int("%d", n)
  # use empty string for NA
  fmt <- ifelse(is.finite(object), fmt, "")
  # convert integers to strings
  sprintf(fmt, object)
}


#' @rdname format_SPSS
#' @export

format_SPSS.numeric <- function(object, digits = 3, p_value = FALSE,
                                check_int = FALSE, ...) {
  # initializations
  n <- length(object)
  digits <- rep_len(digits, n)
  p_value <- rep_len(sapply(p_value, isTRUE), n)
  check_int <- rep_len(sapply(check_int, isTRUE), n)
  # define format with specified number of digits
  fmt <- paste0("%.", digits, "f")
  # use empty string for NA
  finite <- is.finite(object)
  fmt <- ifelse(finite, fmt, "")
  # if requested check for integers and change their format accordingly
  is_int <- finite & check_int & (object == as.integer(object))
  fmt <- ifelse(is_int, "%.0f", fmt)
  # convert numbers to strings
  formatted <- sprintf(fmt, object)
  # if requested format p-value
  # zeros <- rep.int(0, n)
  # below <- finite & !is_int & p_value & (formatted == sprintf(fmt, zeros))
  # formatted[below] <- gsub("^<0.", "<.", paste0("<", 10^(-digits[below])))
  threshold <- 10^(-digits)
  below <- finite & !is_int & p_value & (object >= 0 & object < threshold)
  formatted[below] <- gsub("^<0.", "<.", paste0("<", threshold[below]))
  # replace leading zeros
  formatted <- gsub("^0.", ".", formatted)    # positive numbers
  formatted <- gsub("^-0.", "-.", formatted)  # negative numbers
  # return
  formatted
}


#' @rdname format_SPSS
#' @export

format_SPSS.matrix <- function(object, digits = 3, p_value = FALSE,
                               check_int = FALSE, ...) {
  # initializations
  d <- dim(object)
  p_value <- rep_len(p_value, d[2])
  check_int <- rep_len(check_int, d[2])
  # format each column
  formatted <- vapply(seq_len(d[2]), function(j) {
    format_SPSS(object[, j], digits = digits, p_value = p_value[j],
                check_int = check_int[j])
  }, character(d[1]))
  # add original attributes
  attributes(formatted) <- attributes(object)
  # return formatted matrix
  formatted
}


#' @rdname format_SPSS
#' @export

format_SPSS.data.frame <- function(object, digits = 3, p_value = FALSE,
                                   check_int = FALSE, ...) {
  # initializations
  d <- dim(object)
  p_value <- rep_len(p_value, d[2])
  check_int <- rep_len(check_int, d[2])
  # format each column
  formatted <- mapply(function(v, p, c) {
    format_SPSS(v, digits = digits, p_value = p, check_int = c)
  }, v = object, p = p_value, c = check_int,
  SIMPLIFY = FALSE, USE.NAMES = FALSE)
  # add original attributes
  attributes(formatted) <- attributes(object)
  # return formatted matrix
  formatted
}


#' @rdname format_SPSS
#' @export

formatSPSS <- function(object, ...) {
  .Deprecated("format_SPSS")
  format_SPSS(object, ...)
}
