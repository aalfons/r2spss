# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Wilcoxon Signed Rank and Rank Sum Tests
#'
#' Perform a Wilcoxon signed rank test for a paired sample or a Wilcoxon rank
#' sum test for independent samples on variables of a data set.  The output
#' is printed as a LaTeX table that mimics the look of SPSS output.
#'
#' The \code{print} method first calls the \code{to_SPSS} method followed
#' by \code{\link{to_latex}}.  Further customization can be done by calling
#' those two functions separately, and modifying the object returned by
#' \code{to_SPSS}.
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
#' @param object,x  an object of class \code{"wilcoxon_test_SPSS"} as returned
#' by function \code{wilcoxon_test}.
#' @param statistics  a character string or vector specifying which SPSS tables
#' to produce.  Available options are \code{"ranks"} for a summary of the ranks
#' and \code{"test"} for test results.  For the \code{to_SPSS} method, only one
#' option is allowed (the default is the table of test results), but the
#' \code{print} method allows several options (the default is to print all
#' tables).
#' @param version  a character string specifying whether the table should
#' mimic the content and look of recent SPSS versions (\code{"modern"}) or
#' older versions (<24; \code{"legacy"}).  The main difference in terms of
#' content is that small p-values are displayed differently.
#' @param digits  for the \code{to_SPSS} method, an integer giving the number
#' of digits after the comma to be printed in the SPSS table.  For the
#' \code{print} method, this should be an integer vector of length 2, with the
#' first element corresponding to the number of digits in table with the
#' summary of the ranks, and the second element corresponding to the number of
#' digits in the table for the test.
#' @param \dots additional arguments to be passed down to
#' \code{\link{format_SPSS}}.
#'
#' @return  An object of class \code{"wilcoxon_test_SPSS"} with the following
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
#'   \item{\code{u}}{numeric; the Mann-Whitney U test statistic (only
#'   independent-samples test).}
#'   \item{\code{w}}{numeric; the Wilcoxon rank sum test statistic (only
#'   independent-samples test).}
#'   \item{\code{asymptotic}}{a list containing the results of the Wilcoxon
#'   rank sum test using the normal approximation (only independent-samples
#'   test).}
#'   \item{\code{exact}}{if requested, the corresponding p-value of the exact
#'   Wilcoxon rank sum test test (only independent-samples test).}
#'   \item{\code{group}}{a character string containing the name of the
#'   grouping variable (only independent-samples test).}
#'   \item{\code{type}}{a character string giving the type of Wilcoxon test
#'   performed \code{"paired"} or \code{"independent"}).}
#' }
#'
#' The \code{to_SPSS} method returns an object of class \code{"SPSS_table"}
#' which contains all relevant information in the required format to produce
#' the LaTeX table.  See \code{\link{to_latex}} for possible components and
#' how to further customize the LaTeX table based on the returned object.
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output.
#'
#' @note The Wilcoxon rank sum test also reports the value of the equivalent
#' Mann-Whitney U test statistic.
#'
#' LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
#' may require several LaTeX compilations to be displayed correctly.
#'
#' @author Andreas Alfons
#'
#' @examples
#' ## paired sample
#'
#' # load data
#' data("Exams")
#'
#' # test whether grades differ between the
#' # regular exam and the resit
#' wilcoxon_test(Exams, c("Regular", "Resit"))
#'
#'
#' ## independent samples
#'
#' # load data
#' data("Eredivisie")
#'
#' # test whether market values differ between Dutch and foreign
#' # players
#' wilcoxon_test(Eredivisie, "MarketValue", group = "Foreign")
#'
#' @keywords htest
#'
#' @importFrom stats pnorm pwilcox
#' @export

wilcoxon_test <- function(data, variables, group = NULL, exact = FALSE) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
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
    stat <- data.frame(N = count, "Mean Rank" = sum/count, "Sum of Ranks" = sum,
                       check.names = FALSE, row.names = rn)
    # normal approximation
    n <- length(dnp)
    mu <- n*(n+1) / 4
    nTies <- table(r)
    sigma <- sqrt((n*(n+1)*(2*n+1))/24 - sum(nTies^3-nTies)/48)
    z <- (min(sum) - mu) / sigma
    p <- 2 * min(pnorm(z), pnorm(z, lower.tail=FALSE))
    test <- list(statistic = z, p.value = p)
    # construct object
    out <- list(statistics = stat, test = test, variables = variables[1:2],
                n = length(d), type = "paired")
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
    stat <- data.frame(N = n, "Mean Rank" = sum/n, "Sum of Ranks" = sum,
                       check.names = FALSE, row.names = levels(by))
    # normal approximation
    max <- which.max(sum)
    N <- sum(n)
    mu <- n[max]*(N+1) / 2
    nTies <- table(r)
    sigma <- sqrt(prod(n)/12 * ((N+1) - sum(nTies^3 - nTies)/(N*(N-1))))
    z <- (sum[max] - mu) / sigma
    p <- 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))
    asymptotic <- list(statistic = z, p.value = p)
    # Mann-Whitney U statistic
    u <- sum[max] - n[max]*(n[max]+1)/2
    # sigma <- sqrt((prod(n)*(N+1)) / 12)
    # if requested, compute exact test without correction for ties
    # (can take a long time)
    if (isTRUE(exact)) {
      if (u > prod(n)/2) p <- pwilcox(u-1, n[max], n[-max], lower.tail = FALSE)
      else p <- pwilcox(u, n[max], n[-max])
      exact <- min(2*p, 1)
    } else exact <- NULL
    # construct object
    out <- list(statistics = stat, u = u, w = sum[max],
                asymptotic = asymptotic, exact = exact,
                variables = variables[1], group = group[1],
                type = "independent")
  }
  ## return results
  class(out) <- "wilcoxon_test_SPSS"
  out
}


#' @rdname wilcoxon_test
#' @export

to_SPSS.wilcoxon_test_SPSS <- function(object, statistics = c("test", "ranks"),
                                       version = r2spss_options$get("version"),
                                       digits = NULL, ...) {

  ## initializations
  statistics <- match.arg(statistics)
  if (object$type == "paired") {
    variables <- rev(object$variables)
    label <- paste(variables, collapse = " - ")
  } else label <- object$variables

  if (statistics == "ranks") {

    # initializations
    if (is.null(digits)) digits <- 2
    p <- ncol(object$statistics)
    # prepare necessary information
    if (object$type == "paired") {
      # put table into SPSS format
      N <- object$n
      ties <- N - sum(object$statistics$N)
      ranks <- rbind(object$statistics,
                     Ties = c(ties, rep.int(NA_integer_, p-1)),
                     Total = c(N, rep.int(NA_integer_, p-1)))
      # format table nicely
      formatted <- format_SPSS(ranks, digits = digits, ...)
      # define footnotes
      footnotes <- c(paste(variables, collapse = " < "),
                     paste(variables, collapse = " > "),
                     paste(variables, collapse = " = "))
      footnotes <- data.frame(marker = c("a", "b", "c"), row = 1:3,
                              column = rep.int(1, 3), text = footnotes)
      # construct list containing all necessary information
      spss <- list(table = formatted, main = "Ranks", header = TRUE,
                   label = label, row_names = TRUE, info = 0,
                   footnotes = footnotes)
    } else if (object$type == "independent") {
      # put table into SPSS format
      N <- sum(object$statistics$N)
      ranks <- rbind(object$statistics,
                     Total = c(N, rep.int(NA_integer_, p-1)))
      # format table nicely
      formatted <- format_SPSS(ranks, digits = digits, ...)
      # construct list containing all necessary information
      spss <- list(table = formatted, main = "Ranks", header = TRUE,
                   label = label, row_names = TRUE, info = 0)
    } else stop("type of test not supported")

  } else if (statistics == "test") {

    # initializations
    if (is.null(digits)) digits <- 3
    version <- match.arg(version, choices = get_version_values())
    legacy <- version == "legacy"
    # prepare necessary information
    if (object$type == "paired") {
      # extract results
      rn <- c("Z", "Asymp. Sig. (2-tailed)")
      values <- unlist(object$test)
      # format results nicely
      args <- list(values, digits = digits, ...)
      if (is.null(args$p_value)) args$p_value <- c(FALSE, !legacy)
      formatted <- do.call(format_SPSS, args)
      # put test results into SPSS format
      test <- data.frame(formatted, row.names = rn)
      names(test) <- label
      # define footnotes
      footnotes <- data.frame(marker = c("a", "b"), row = c("main", 1),
                              column = c(NA_integer_, 1),
                              text = c("Wilcoxon Signed Ranks Test",
                                       "Based on positive ranks."))
      # construct list containing all necessary information
      spss <- list(table = test, main = "Test Statistics",
                   header = TRUE, row_names = TRUE, info = 0,
                   footnotes = footnotes, version = version)
    } else if (object$type == "independent") {
      # initializations
      have_exact <- !is.null(object$exact)
      # extract results
      rn <- c("Mann-Whitney U", "Wilcoxon W", "Z", "Asymp. Sig. (2-tailed)")
      values <- c(object$u, object$w, unlist(object$asymptotic))
      p_value <- c(FALSE, FALSE, FALSE, !legacy)
      if (have_exact) {
        rn <- c(rn, "Exact Sig. [2*(1-tailed Sig.)]")
        values <- c(values, object$exact)
        p_value <- c(p_value, !legacy)
      }
      # format results nicely
      args <- list(values, digits = digits, ...)
      if (is.null(args$p_value)) args$p_value <- p_value
      formatted <- do.call(format_SPSS, args)
      # put test results into SPSS format
      test <- data.frame(formatted, row.names = rn)
      names(test) <- label
      # define footnotes
      marker <- "a"
      row <- "main"
      column <- NA_integer_
      footnote <- paste("Grouping Variable:", object$group)
      if (have_exact) {
        marker <- c(marker, "b")
        row <- c(row, 5)
        column <- c(column, 1)
        footnote <- c(footnote, "Not corrected for ties.")
      }
      footnotes <- data.frame(marker = marker, row = row, column = column,
                              text = footnote)
      # construct list containing all necessary information
      spss <- list(table = test, main = "Test Statistics",
                   header = TRUE, row_names = TRUE, info = 0,
                   footnotes = footnotes, version = version)
    } else stop("type of test not supported")

  } else stop ("type of 'statistics' not supported")  # shouldn't happen

  # add class and return object
  class(spss) <- "SPSS_table"
  spss

}


#' @rdname wilcoxon_test
#' @export

print.wilcoxon_test_SPSS <- function(x, statistics = c("ranks", "test"),
                                     version = r2spss_options$get("version"),
                                     digits = 2:3, ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  version <- match.arg(version, choices = get_version_values())
  digits <- rep_len(digits, 2)

  ## print LaTeX table for ranks
  if ("ranks" %in% statistics) {
    cat("\n")
    # put table into SPSS format
    spss <- to_SPSS(x, digits = digits[1], statistics = "ranks",
                    version = version, ...)
    # print LaTeX table
    to_latex(spss, version = version)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put test results into SPSS format
    spss <- to_SPSS(x, digits = digits[2], statistics = "test",
                    version = version, ...)
    # print LaTeX table
    to_latex(spss, version = version)
    cat("\n")
  }

}


#' @rdname wilcoxon_test
#' @export

wilcoxonTest <- function(data, variables, group = NULL, exact = FALSE) {
  .Deprecated("wilcoxon_test")
  wilcoxon_test(data, variables = variables, group = group, exact = exact)
}
