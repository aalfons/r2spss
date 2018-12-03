# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Sign Test
#'
#' Perform a sign test for paired samples on variables of a data set.  The
#' output is printed as a LaTeX table that mimics the look of SPSS output
#' (version <24).
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying two numeric variables
#' containing the paired observations.
#' @param exact  a logical indicating whether or not to include the exact
#' p-value using the binomial distribution.  Note that the p-value using the
#' normal approximation is always reported.
#'
#' @return  An object of class \code{signTest}.  The \code{print} method
#' produces a LaTeX table that mimics the look of SPSS output (version <24).
#'
#' @author Andreas Alfons
#'
#' @keywords htest
#'
#' @importFrom stats dbinom pbinom pnorm
#' @export

signTest <- function(data, variables, exact = FALSE) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) < 2) stop("two variables to test must be specified")
  # compute differences
  d <- data[, variables[2]] - data[, variables[1]]
  ok <- is.finite(d)
  d <- d[ok]
  # compute signs
  dnp <- d[d != 0]
  s <- sign(dnp)
  # compute statistics
  negative <- s < 0
  positive <- s > 0
  count <- c(sum(negative), sum(positive))
  if (any(count == 0)) stop("all differences negative or positive")
  rn <- c("Negative Differences", "Positive Differences")
  stat <- data.frame(N=count, check.names=FALSE, row.names=rn)
  # normal approximation
  n <- length(dnp)
  mu <- n * 0.5
  sigma <- sqrt(n) * 0.5
  min <- which.min(count)
  z <- (count[min] - mu + 0.5) / sigma  # continuity correction
  p <- 2 * min(pnorm(z), pnorm(z, lower.tail=FALSE))
  asymptotic <- list(statistic=z, p.value=p)
  # exact test
  if (isTRUE(exact)) {
    p <- pbinom(count[min], n, 0.5)
    exact <- c(min(2*p, 1), p, dbinom(count[min], n, 0.5))
  } else exact <- NULL
  ## return results
  out <- list(statistics=stat, asymptotic=asymptotic, exact=exact,
              variables=variables[1:2], n=length(d))
  class(out) <- "signTest"
  out
}


#' @rdname signTest
#'
#' @param x  an object of class \code{"signTest"} as returned by function
#' \code{signTest}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the LaTeX tables.
#' @param statistics  a character vector specifying which LaTeX tables should
#' be printed.  Available options are \code{"frequencies"} for a summary of the
#' frequencies and \code{"test"} for test results.  The default is to print
#' both tables.
#' @param \dots currently ignored.
#'
#' @export

print.signTest <- function(x, digits = 3, statistics = c("frequencies", "test"),
                           ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)

  ## print LaTeX table for ranks
  if ("frequencies" %in% statistics) {
    formatted <- formatSPSS(x$statistics)
    rownames(formatted) <- paste0(rownames(formatted), "$^\\text{", c("a", "b"), "}$")
    # print LaTeX table
    cat("\n")
    cat("\\begin{tabular}{|ll|r|}\n")
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{3}{c}{\\textbf{Frequencies}} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & & \\multicolumn{1}{|c|}{N} \\\\\n")
    cat("\\hline\n")
    cat(x$variables[2], "-", x$variables[1], "&", rownames(formatted)[1], "&", formatted[1, "N"], "\\\\\n")
    cat(" &", rownames(formatted)[2], "&", formatted[2, "N"], "\\\\\n")
    cat(" & Ties$^\\text{c}$ & ", x$n - sum(x$statistics$N), " \\\\\n", sep="")
    cat(" & Total &", x$n, "\\\\\n")
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\multicolumn{3}{l}{", "a. ", x$variables[2], " < ", x$variables[1], "} \\\\\n", sep="")
    cat("\\multicolumn{3}{l}{", "b. ", x$variables[2], " > ", x$variables[1], "} \\\\\n", sep="")
    cat("\\multicolumn{3}{l}{", "c. ", x$variables[2], " = ", x$variables[1], "} \\\\\n", sep="")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {

    ## collect output for test
    test <- c(x$asymptotic$statistic, x$asymptotic$p.value, x$exact)
    formatted <- formatSPSS(test, digits=digits)

    ## print LaTeX table
    if (count == 0) cat("\n")
    cat("\\begin{tabular}{|l|r|}\n")
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{2}{c}{\\textbf{Test Statistics}$^{\\text{a}}$} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & \\multicolumn{1}{|c|}{", paste(x$variables[2], "-", x$variables[1]), "} \\\\\n", sep="")
    cat("\\hline\n")
    cat("Z &", formatted[1], "\\\\\n")
    cat("Asymp. Sig. (2-tailed) &", formatted[2], "\\\\\n")
    if (!is.null(x$exact)) {
      cat("Exact Sig. (2-tailed) &", formatted[3], "\\\\\n")
      cat("Exact Sig. (1-tailed) &", formatted[4], "\\\\\n")
      cat("Point Probability &", formatted[5], "\\\\\n")
    }
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\multicolumn{2}{l}{a. Sign Test} \\\\\n")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
  }
}
