# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Trimmed mean
#'
#' Compute the trimmed mean.  This function differs from the implementation of
#' the trimmed mean in the base \R function \code{\link{mean}} in the following
#' ways.  While \code{mean} always rounds down the number of observations to
#' be trimmed, this function rounds to the nearest integer.  In addition,
#' \code{mean} implements proper \code{NA} handling, whereas this function
#' assumes that there are no missing values and may fail in their presence.
#'
#' The main purpose of this function is to reproduce SPSS results for Levene's
#' test on homogeneity of the variances based on the trimmed mean (see
#' \code{\link{ANOVA}}), which are slightly too far off when using the base
#' \R function \code{\link{mean}}.  Rounding the number of observations to be
#' trimmed to the nearest integer brings the results closer to those of SPSS,
#' but they are still not identical.
#'
#' @param x  a numeric vector.
#' @param trim  numeric; the fraction of observations to be trimmed from each
#' tail of \code{x} before computing the mean (defaults to 0.05).
#'
#' @value  The trimmed mean of the values in \code{x} as a single numeric value.
#'
#' @seealso \code{\link{mean}}
#'
#' @author Andreas Alfons
#'
#' @examples
#' x <- c(0:10, 50)
#'
#' # trimmedMean() rounds number of observations
#' # to be trimmed to the nearest integer
#' trimmedMean(x, trim = 0.05)
#'
#' # base R function mean() rounds down number of
#' # observations to be trimmed
#' mean(x, trim = 0.05)
#' mean(x)
#'
#' @keywords arith
#'
#' @importFrom stats median
#' @export

trimmedMean <- function(x, trim = 0.05) {
  # number of observations
  n <- length(x)
  # trim observations from both ends
  if (trim > 0 && n > 0) {
    # low <- floor(n * trim) + 1  # default R behavior
    low <- round(n * trim) + 1    #
    high <- n + 1 - low
    if (low <= high) {
      x <- sort.int(x, partial = unique(c(low, high)))[low:high]
    } else median(x, na.rm = FALSE)
  }
  # comupte mean
  mean(x, na.rm = FALSE)
}
