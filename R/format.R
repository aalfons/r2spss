#' @export
formatSPSS <- function(x, ...) UseMethod("formatSPSS")

#' @export
formatSPSS.default <- function(x, ...) as.character(x)

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

#' @export
formatSPSS.matrix <- function(x, ...) {
  # format as vector and add original attributes
  formatted <- formatSPSS(as.vector(x), ...)
  attributes(formatted) <- attributes(x)
  # return formatted matrix
  formatted
}

#' @export
formatSPSS.data.frame <- function(x, ...) {
  # format each variable
  formatted <- lapply(x, formatSPSS, ...)
  formatted <- do.call(cbind, formatted)
  # add row names
  rownames(formatted) <- row.names(x)
  formatted
}
