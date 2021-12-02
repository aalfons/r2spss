# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Descriptive Statistics
#'
#' Compute descriptive statistics of numeric variables of a data set (number of
#' observations, minimum, maximum, mean, standard deviaiton).  The output is
#' printed as a LaTeX table that mimics the look of SPSS output (version <24).
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying numeric variables for which
#' to compute descriptive statistics.
#'
#' @return
#' An object of class \code{"descriptivesSPSS"} with the following components:
#' \describe{
#'   \item{\code{classes}}{a character vector giving the (first) class of the
#'   variables of interest.}
#'   \item{\code{descriptives}}{a data frame containing the descriptive
#'   statistics.}
#'   \item{\code{n}}{an integer giving the number of observations.}
#' }
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output (version <24).
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#'
#' # compute descriptive statistics for market value and age
#' descriptives(Eredivisie, c("MarketValue", "Age"))
#'
#' @keywords univar
#'
#' @importFrom stats complete.cases sd
#' @export

descriptives <- function(data, variables) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) == 0) stop("variables must be specified")
  x <- data[, variables, drop=FALSE]
  # initializations
  classes <- vapply(x, function(x) class(x)[1], character(1))
  n <- sum(complete.cases(x))
  # compute minimum, maximum, mean and standard deviation for each variable
  desc <- do.call(rbind, lapply(x, .descriptives))
  row.names(desc) <- variables
  # return descriptives
  out <- list(classes=classes, descriptives=desc, n=n)
  class(out) <- "descriptivesSPSS"
  out
}

# compute minimum, maximum, mean and standard deviation for a variable
.descriptives <- function(x) {
  # initializations
  ok <- is.finite(x)
  x <- x[ok]
  # compute descriptives
  n <- length(x)
  range <- range(x)
  mean <- mean(x)
  sd <- sd(x)
  # return data frame
  data.frame(N=n, Minimum=range[1], Maximum=range[2], Mean=mean,
             "Std. Deviation"=sd, check.names=FALSE)
}


## convert R results to all necessary information for SPSS-like table
#' @export

toSPSS.descriptivesSPSS <- function(object, digits = 2, ...) {
  # put table of results into SPSS format
  p <- ncol(object$descriptives)
  descriptives <- rbind(object$descriptives,
                        "Valid N (listwise)" = c(object$n, rep.int(NA, p)))
  # define header with line breaks
  colNames <- names(descriptives)
  header <- c("", wrapText(colNames, limit = 10))
  # format table nicely
  args <- list(descriptives, digits = digits, ...)
  if (is.null(args$checkInt)) {
    args$checkInt <- colNames %in% c("Minimum", "Maximum")
  }
  formatted <- do.call(formatSPSS, args)
  # construct return object
  spss <- list(table = formatted, main = "Descriptive Statistics",
               header = header, rowNames = TRUE, info = 0)
  class(spss) <- "SPSSTable"
  spss
}


#' @rdname descriptives
#'
#' @param x  an object of class \code{"descriptivesSPSS"} as returned by
#' function \code{descriptives}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the LaTeX table.
#' @param \dots currently ignored.
#'
#' @export

print.descriptivesSPSS <- function(x, theme = c("modern", "legacy"), ...) {
  # initializations
  theme <- match.arg(theme)
  # put table of results into SPSS format
  spss <- toSPSS(x, ...)
  # print LaTeX table
  toLatex(spss, theme = theme)
}
