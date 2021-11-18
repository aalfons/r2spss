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


#' @rdname descriptives
#'
#' @param x  an object of class \code{"descriptives"} as returned by function
#' \code{descriptives}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the LaTeX table.
#' @param \dots currently ignored.
#'
#' @export

print.descriptivesSPSS <- function(x, digits = 2, ...) {
  # format descriptives
  d <- ifelse(x$classes == "integer", 0, digits)
  formatted <- cbind(
    formatSPSS(x$descriptives[, "N"]),
    formatSPSS(x$descriptives[, c("Minimum", "Maximum")], digits=d),
    formatSPSS(x$descriptives[, c("Mean", "Std. Deviation")], digits=digits)
  )
  # initialize LaTeX table
  cat("\\begin{tabular}{|l|r|r|r|r|r|}\n")
  # print table header
  cat("\\noalign{\\smallskip}\n")
  cat("\\multicolumn{6}{c}{\\textbf{Descriptive Statistics}} \\\\\n")
  cat("\\noalign{\\smallskip}\\hline\n")
  cat(" & & & & & \\multicolumn{1}{|c|}{Std.} \\\\\n")
  cat(" & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Minimum} & \\multicolumn{1}{|c|}{Maximum} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} \\\\\n")
  cat("\\hline\n")
  # print descriptives for each variable
  for (variable in rownames(formatted)) {
    cat(variable, "&", paste0(formatted[variable, ], collapse=" & "), "\\\\\n")
  }
  # print complete cases
  cat("Valid N (listwise) &", x$n, "& & & & \\\\\n")
  # finalize LaTeX table
  cat("\\hline\\noalign{\\smallskip}\n")
  cat("\\end{tabular}\n")
}
