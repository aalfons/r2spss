# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Wilcoxon Signed Rank and Rank Sum Tests
#'
#' Perform a Wilcoxon signed rank test for paired samples or a Wilcoxon rank
#' sum test for independent samples on variables of a data set.  The output
#' is printed as a LaTeX table that mimics the look of SPSS output (version
#' <24).
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying numeric variable(s) to be
#' used.  If \code{group} is \code{NULL}, the Wilcoxon signed rank test is
#' performed and this should be a character vector specifying two numeric
#' variables which contain the paired observations.  If a grouping variable is
#' specified in \code{group}, the Wilcoxon rank sum test is performed and this
#' should be a character string specifying the numeric variable of interest.
#' @param group  a character string specifying a grouping variable for the
#' Wilcoxon rank sum test, or \code{NULL}.
#' @param exact  a logical indicating whether the Wilcoxon rank sum test
#' should also return the p-value of the exact test.  The default is
#' \code{FALSE}.  Note that the p-value of the asymptotic test is always
#' returned.
#'
#' @return  An object of class \code{"wilcoxonTestSPSS"} with the following
#' components:
#' \describe{
#'   \item{\code{statistics}}{a data frame containing the relevant information
#'   on the ranks.}
#'   \item{\code{test}}{a list containing the results of the Wilcoxon signed
#'   rank test (only paired-sample test).}
#'   \item{\code{variables}}{a character vector containing the name(s) of the
#'   relevant numeric variable(s).}
#'   \item{\code{n}}{an integer giving the number of observations (only
#'   paired-sample test).}
#'   \item{\code{w}}{numeric; the Wilcoxon rank sum test statistic (only
#'   independent-samples test).}
#'   \item{\code{asymptotic}}{a list containing the results of the Wilcoxon
#'   rank sum test using the normal approximation (only independent-samples
#'   test).}
#'   \item{\code{exact}}{a list containing the test statistic of the exact
#'   Wilcoxon rank sum test test, and if requested the corresponding p-value
#'   (only independent-samples test).}
#'   \item{\code{group}}{a character string containing the name of the
#'   grouping variable (only independent-samples test).}
#'   \item{\code{type}}{a character string giving the type of Wilcoxon test
#'   performed \code{"paired"} or \code{"independent"}).}
#' }
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output (version <24).
#'
#' @note The Wilcoxon rank sum test also reports the value of the equivalent
#' Mann-Whitney U test statistic.
#'
#' @author Andreas Alfons
#'
#' @examples
#' ## independent samples
#'
#' # load data
#' data("Eredivisie")
#'
#' # test whether market values differ between Dutch and foreign
#' # players
#' wilcoxonTest(Eredivisie, "MarketValue", group = "Foreign")
#'
#' @keywords htest
#'
#' @importFrom stats pnorm pwilcox
#' @export

wilcoxonTest <- function(data, variables, group = NULL, exact = FALSE) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  exact_p_value <- isTRUE(exact)
  ## select test
  if (length(group) == 0) {
    ## signed rank test
    if (length(variables) < 2) stop("two variables to test must be specified")
    # compute differences
    d <- data[, variables[2]] - data[, variables[1]]
    ok <- is.finite(d)
    d <- d[ok]
    # compute ranks
    dnp <- d[d != 0]
    r <- rank(abs(dnp))
    # compute statistics
    negative <- dnp < 0
    positive <- dnp > 0
    count <- c(sum(negative), sum(positive))
    if (any(count == 0)) stop("all differences negative or positive")
    sum <- c(sum(r[negative]), sum(r[positive]))
    rn <- c("Negative Ranks", "Positive Ranks")
    stat <- data.frame(N=count, "Mean Rank"=sum/count, "Sum of Ranks"=sum,
                       check.names=FALSE, row.names=rn)
    # normal approximation
    n <- length(dnp)
    mu <- n*(n+1) / 4
    nTies <- table(r)
    sigma <- sqrt((n*(n+1)*(2*n+1))/24 - sum(nTies^3-nTies)/48)
    z <- (min(sum) - mu) / sigma
    p <- 2 * min(pnorm(z), pnorm(z, lower.tail=FALSE))
    test <- list(statistic=z, p.value=p)
    # construct object
    out <- list(statistics=stat, test=test, variables=variables[1:2],
                n=length(d), type="paired")
  } else {
    ## rank sum test
    if (length(variables) == 0) stop("a variable to test must be specified")
    variable <- data[, variables[1]]
    by <- as.factor(data[, group[1]])
    if (nlevels(by) != 2) {
      stop("rank sum test requires exactly two groups")
    }
    ok <- is.finite(variable) & !is.na(by)
    variable <- variable[ok]
    by <- by[ok]
    # compute ranks
    r <- rank(variable)
    # compute statistics
    first <- by == levels(by)[1]
    second <- by == levels(by)[2]
    n <- c(sum(first), sum(second))
    if (any(n == 0)) stop("all differences negative or positive")
    sum <- c(sum(r[first]), sum(r[second]))
    stat <- data.frame(N=n, "Mean Rank"=sum/n, "Sum of Ranks"=sum,
                       check.names=FALSE, row.names=levels(by))
    # normal approximation
    max <- which.max(sum)
    N <- sum(n)
    mu <- n[max]*(N+1) / 2
    nTies <- table(r)
    sigma <- sqrt(prod(n)/12 * ((N+1) - sum(nTies^3 - nTies)/(N*(N-1))))
    z <- (sum[max] - mu) / sigma
    p <- 2 * min(pnorm(z), pnorm(z, lower.tail=FALSE))
    asymptotic <- list(statistic=z, p.value=p)
    # exact test without correction for ties
    u <- sum[max] - n[max]*(n[max]+1)/2
    # sigma <- sqrt((prod(n)*(N+1)) / 12)
    exact <- list(statistic=u)
    if (exact_p_value) {
      # compute p-value only if requested, as it can take a long time
      if (u > prod(n)/2) p <- pwilcox(u-1, n[max], n[-max], lower.tail=FALSE)
      else p <- pwilcox(u, n[max], n[-max])
      p <- min(2*p, 1)
      exact$p.value <- p
    }
    # construct object
    out <- list(statistics=stat, w=sum[max], asymptotic=asymptotic, exact=exact,
                variables=variables[1], group=group[1], type="independent")
  }
  ## return results
  class(out) <- "wilcoxonTestSPSS"
  out
}


#' @rdname wilcoxonTest
#'
#' @param x  an object of class \code{"wilcoxonTestSPSS"} as returned by
#' function \code{wilcoxonTest}.
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

print.wilcoxonTestSPSS <- function(x, digits = 2:3,
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
    if (x$type == "paired") {
      formatted[, "N"] <- paste0(formatted[, "N"], "$^\\text{", c("a", "b"), "}$")
      cat("\\begin{tabular}{|ll|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{Ranks}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean Rank} & \\multicolumn{1}{|c|}{Sum of Ranks} \\\\\n")
      cat("\\hline\n")
      cat(x$variables[2], "-", x$variables[1], "&", rownames(formatted)[1], "&", paste0(formatted[1, ], collapse=" & "), "\\\\\n")
      cat(" &", rownames(formatted)[2], "&", paste(formatted[2, ], collapse=" & "), "\\\\\n")
      cat(" & Ties & ", x$n - sum(x$statistics$N), "$^\\text{c}$ & & \\\\\n", sep="")
      cat(" & Total &", x$n, "& & \\\\\n")
      cat("\\hline\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{l}{", "a. ", x$variables[2], " < ", x$variables[1], "} \\\\\n", sep="")
      cat("\\multicolumn{5}{l}{", "b. ", x$variables[2], " > ", x$variables[1], "} \\\\\n", sep="")
      cat("\\multicolumn{5}{l}{", "c. ", x$variables[2], " = ", x$variables[1], "} \\\\\n", sep="")
    } else if (x$type == "independent") {
      cat("\\begin{tabular}{|ll|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{Ranks}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" &", x$group, "& \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean Rank} & \\multicolumn{1}{|c|}{Sum of Ranks} \\\\\n")
      cat("\\hline\n")
      cat(x$variables, "&", rownames(formatted)[1], "&", paste0(formatted[1, ], collapse=" & "), "\\\\\n")
      cat(" &", rownames(formatted)[2], "&", paste0(formatted[2, ], collapse=" & "), "\\\\\n")
      cat(" & Total &", sum(x$statistics$N), "& & \\\\\n")
      cat("\\hline\\noalign{\\smallskip}\n")
    } else stop("type of test not supported")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {

    ## collect output for test
    if (x$type == "paired") {
      test <- c(x$test$statistic, x$test$p.value)
      formatted <- formatSPSS(test, digits=digits[2])
      min <- which.min(x$statistics[, "Sum of Ranks"])
    } else if (x$type == "independent") {
      have_exact <- !is.null(x$exact$p.value)
      test <- c(x$exact$statistic, x$w, x$asymptotic$statistic,
                x$asymptotic$p.value, x$exact$p.value)
      formatted <- formatSPSS(test, digits=digits[2])
    } else stop("type of test not supported")

    ## print LaTeX table
    if (count == 0) cat("\n")
    if (x$type == "paired") {
      cat("\\begin{tabular}{|l|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{c}{\\textbf{Test Statistics}$^{\\text{a}}$} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & \\multicolumn{1}{|c|}{", paste(x$variables[2], "-", x$variables[1]), "} \\\\\n", sep="")
      cat("\\hline\n")
      cat("Z & ", formatted[1], "$^\\text{b}$ \\\\\n", sep="")
      cat("Asymp. Sig. (2-tailed) &", formatted[2], "\\\\\n")
      cat("\\hline\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{l}{a. Wilcoxon Signed Ranks Test} \\\\\n")
      cat("\\multicolumn{2}{l}{b. Based on", c("negative", "positive")[min], "ranks.} \\\\\n")
    } else if (x$type == "independent") {
      cat("\\begin{tabular}{|l|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{c}{\\textbf{Test Statistics}$^{\\text{a}}$} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & \\multicolumn{1}{|c|}{", x$variables, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat("Mann-Whitney U &", formatted[1], "\\\\\n")
      cat("Wilcoxon W &", formatted[2], "\\\\\n")
      cat("Z &", formatted[3], "\\\\\n")
      cat("Asymp. Sig. (2-tailed) &", formatted[4], "\\\\\n")
      if (have_exact) {
        cat("Exact Sig. [2*(1-tailed Sig.)] &", formatted[5], "$^\\text{b}$ \\\\\n", sep="")
      }
      cat("\\hline\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{l}{a. Grouping Variable: ", x$group, "} \\\\\n", sep="")
      cat("\\multicolumn{2}{l}{b. Not corrected for ties.} \\\\\n")
    } else stop("type of test not supported")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
  }
}
