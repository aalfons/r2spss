# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Kruskal-Wallis Test
#'
#' Perform a Kruskal-Wallis test on variables of a data set.  The output is
#' printed as a LaTeX table that mimics the look of SPSS output (version <24).
#'
#' @param data  a data frame containing the variables.
#' @param variable  a character string specifying the numeric variable of
#' interest.
#' @param group  a character string specifying a grouping variable.
#'
#' @return
#' An object of class \code{"kruskalTestSPSS"} with the following components:
#' \describe{
#'   \item{\code{statistics}}{a data frame containing information on the
#'   per-group mean ranks.}
#'   \item{\code{test}}{a list containing the results of the Kruskal-Wallis
#'   test.}
#'   \item{\code{variable}}{a character string giving the numeric variable of
#'   interest.}
#'   \item{\code{group}}{a character vector giving the grouping variable.}
#' }
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output (version <24).
#'
#' @author Andreas Alfons
#'
#' @keywords htest
#'
#' @importFrom stats pchisq
#' @export

kruskalTest <- function(data, variable, group) {
  ## initializations
  data <- as.data.frame(data)
  variable <- as.character(variable)
  group <- as.character(group)
  if (length(variable) == 0) stop("a variable to test must be specified")
  if (length(group) == 0) stop("a grouping variable must be specified")
  x <- data[, variable[1]]
  by <- as.factor(data[, group[1]])
  if (nlevels(by) < 2) {
    stop("Kruskal-Wallis test requires at least two groups")
  }
  ok <- is.finite(x) & !is.na(by)
  x <- x[ok]
  by <- by[ok]
  r <- rank(x)
  # compute statistics
  n <- tapply(r, by, length)
  if (any(is.na(n))) stop("unused factor levels")
  sum <- tapply(r, by, sum)
  stat <- data.frame(N=n, "Mean Rank"=sum/n, check.names=FALSE,
                     row.names=levels(by))
  # chi-square approximation
  N <- sum(n)
  nTies <- table(r)
  h <- (12*sum(sum^2/n)/(N*(N+1)) - 3*(N+1)) / (1-sum(nTies^3-nTies)/(N^3-N))
  df <- nlevels(by) - 1
  p <- pchisq(h, df, lower.tail=FALSE)
  test <- list(statistic=h, parameter=df, p.value=p)
  ## return results
  out <- list(statistics=stat, test=test, variable=variable[1],
              group=group[1])
  class(out) <- "kruskalTestSPSS"
  out
}


#' @rdname kruskalTest
#'
#' @param x  an object of class \code{"kruskalTestSPSS"} as returned by
#' function \code{kruskalTest}.
#' @param digits  an integer vector giving the number of digits after the comma
#' to be printed in the LaTeX tables.  The first element corresponds to the
#' number of digits in table with the summary of the ranks, and the second
#' element corresponds to the number of digits in the table for the test.
#' @param statistics  a character vector specifying which LaTeX tables should
#' be printed.  Available options are \code{"ranks"} for a summary of the ranks
#' and \code{"test"} for test results.  The default is to print both tables.
#' @param \dots currently ignored.
#'
#' @export

print.kruskalTestSPSS <- function(x, digits = 2:3,
                                  statistics = c("ranks", "test"),
                                  ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)

  ## print LaTeX table for ranks
  if ("ranks" %in% statistics) {
    formatted <- formatSPSS(x$statistics, digits=digits[1])
    # print LaTeX table
    cat("\n")
    cat("\\begin{tabular}{|ll|r|r|}\n")
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{4}{c}{\\textbf{Ranks}} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" &", x$group, "& \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean Rank} \\\\\n")
    cat("\\hline\n")
    cat(x$variable)
    for (rn in rownames(formatted)) {
      cat(" &", rn, "&", paste0(formatted[rn, ], collapse=" & "), "\\\\\n")
    }
    cat(" & Total &", sum(x$statistics$N), "& \\\\\n")
    cat("\\hline\\noalign{\\smallskip}\n")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {

    ## collect output for test
    test <- c(x$test$statistic, x$test$p.value)
    formatted <- formatSPSS(test, digits=digits[2])

    ## print LaTeX table
    if (count == 0) cat("\n")
    cat("\\begin{tabular}{|l|r|}\n")
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{2}{c}{\\textbf{Test Statistics}$^{\\text{a,b}}$} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & \\multicolumn{1}{|c|}{", x$variable, "} \\\\\n", sep="")
    cat("\\hline\n")
    cat("Chi-Square &", formatted[1], "\\\\\n")
    cat("df &", x$test$parameter, "\\\\\n")
    cat("Asymp. Sig. &", formatted[2], "\\\\\n")
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\multicolumn{2}{l}{a. Kruskal Wallis Test} \\\\\n")
    cat("\\multicolumn{2}{l}{b. Grouping Variable: ", x$group, "} \\\\\n", sep="")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
  }
}
