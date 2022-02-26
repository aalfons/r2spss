# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Sign Test
#'
#' Perform a sign test for a paired sample on variables of a data set.  The
#' output is printed as a LaTeX table that mimics the look of SPSS output.
#'
#' The \code{print} method first calls the \code{toSPSS} method followed by
#' \code{\link[=toLatex.SPSSTable]{toLatex}}.  Further customization can be
#' done by calling those two functions separately, and modifying the object
#' returned by \code{toSPSS}.
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying two numeric variables
#' containing the paired observations.
#' @param exact  a logical indicating whether or not to include the exact
#' p-value using the binomial distribution.  Note that the p-value using the
#' normal approximation is always reported.
#' @param object,x  an object of class \code{"sign_test_SPSS"} as returned by
#' function \code{sign_test}.
#' @param statistics  a character string or vector specifying which SPSS tables
#' to produce.   Available options are \code{"frequencies"} for a summary of
#' the frequencies and \code{"test"} for test results.  For the \code{toSPSS}
#' method, only one option is allowed (the default is the table of test
#' results), but the \code{print} method allows several options (the default
#' is to print all tables).
#' @param version  a character string specifying whether the table should
#' mimic the content and look of recent SPSS versions (\code{"modern"}) or
#' older versions (<24; \code{"legacy"}).  The main difference in terms of
#' content is that small p-values are displayed differently.
#' @param \dots additional arguments to be passed down to
#' \code{\link{format_SPSS}}.
#'
#' @return  An object of class \code{"sign_test_SPSS"} with the following
#' components:
#' \describe{
#'   \item{\code{statistics}}{a data frame containing information on the
#'   number of observations with negative and positive differences.}
#'   \item{\code{asymptotic}}{a list containing the results of the test using
#'   the normal approximation.}
#'   \item{\code{exact}}{if requested, a numeric vector containing the exact
#'   two-sided p-value, one-sided p-value, and point probability using the
#'   binomial distribution.}
#'   \item{\code{variables}}{a character vector containing the names of the two
#'   numeric variables with the paired observations.}
#'   \item{\code{n}}{an integer giving the number of observations.}
#' }
#'
#' The \code{toSPSS} method returns an object of class \code{"SPSSTable"}
#' which contains all relevant information in the required format to produce
#' the LaTeX table.  See \code{\link[=toLatex.SPSSTable]{toLatex}} for possible
#' components and how to further customize the LaTeX table based on the
#' returned object.
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output.
#'
#' @note
#' LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
#' may require several LaTeX compilations to be displayed correctly.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Exams")
#'
#' # test whether grades differ between the
#' # regular exam and the resit
#' sign_test(Exams, c("Regular", "Resit"))
#'
#' @keywords htest
#'
#' @importFrom stats dbinom pbinom pnorm
#' @export

sign_test <- function(data, variables, exact = FALSE) {
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
  stat <- data.frame(N = count, check.names = FALSE, row.names = rn)
  # normal approximation
  n <- length(dnp)
  mu <- n * 0.5
  sigma <- sqrt(n) * 0.5
  min <- which.min(count)
  z <- (count[min] - mu + 0.5) / sigma  # continuity correction
  p <- 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))
  asymptotic <- list(statistic = z, p.value = p)
  # exact test
  if (isTRUE(exact)) {
    p <- pbinom(count[min], n, 0.5)
    exact <- c(min(2*p, 1), p, dbinom(count[min], n, 0.5))
  } else exact <- NULL
  ## return results
  out <- list(statistics = stat, asymptotic = asymptotic, exact = exact,
              variables = variables[1:2], n = length(d))
  class(out) <- "sign_test_SPSS"
  out
}


#' @rdname sign_test
#' @export

toSPSS.sign_test_SPSS <- function(object,
                                  statistics = c("test", "frequencies"),
                                  version = r2spss_options$get("version"),
                                  ...) {

  ## initializations
  statistics <- match.arg(statistics)
  label <- paste(rev(object$variables), collapse = " - ")

  if (statistics == "frequencies") {

    # put table into SPSS format
    N <- object$n
    ties <- N - sum(object$statistics$N)
    frequencies <- data.frame(Group = c(rownames(object$statistics),
                                        "Ties", "Total"),
                              N = c(object$statistics$N, ties, N))
    # format table nicely
    formatted <- format_SPSS(frequencies, ...)
    # define header
    header <- c("", "", "N")
    # define footnotes
    footnotes <- c(paste(object$variables, collapse = " < "),
                   paste(object$variables, collapse = " > "),
                   paste(object$variables, collapse = " = "))
    footnotes <- data.frame(marker = c("a", "b", "c"), row = 1:3,
                            column = rep.int(1, 3), text = footnotes)
    # construct list containing all necessary information
    spss <- list(table = formatted, main = "Frequencies",
                 header = header, label = label, rowNames = FALSE,
                 info = 1, footnotes = footnotes)

  } else if (statistics == "test") {

    # initializations
    version <- match.arg(version, choices = get_version_values())
    legacy <- version == "legacy"
    # extract results
    rn <- c("Z", "Asymp. Sig. (2-tailed)")
    values <- unlist(object$asymptotic)
    p_value <- c(FALSE, !legacy)
    if (!is.null(object$exact)) {
      rn <- c(rn, sprintf("Exact Sig. (%d-tailed)", 2:1), "Point probability")
      values <- c(values, object$exact)
      p_value <- c(p_value, !legacy, !legacy, FALSE)
    }
    # format results nicely
    args <- list(values, ...)
    if (is.null(args$p_value)) args$p_value <- p_value
    formatted <- do.call(format_SPSS, args)
    # put test results into SPSS format
    test <- data.frame(formatted, row.names = rn)
    names(test) <- label
    # define footnotes
    footnotes <- data.frame(marker = "a", row = "main",
                            column = NA_integer_,
                            text = "Sign Test")
    # construct list containing all necessary information
    spss <- list(table = test, main = "Test Statistics",
                 header = TRUE, rowNames = TRUE, info = 0,
                 footnotes = footnotes, version = version)

  } else stop ("type of 'statistics' not supported")  # shouldn't happen

  # add class and return object
  class(spss) <- "SPSSTable"
  spss

}

#' @rdname sign_test
#' @export

print.sign_test_SPSS <- function(x, statistics = c("frequencies", "test"),
                                 version = r2spss_options$get("version"), ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  version <- match.arg(version, choices = get_version_values())

  ## print LaTeX table for ranks
  if ("frequencies" %in% statistics) {
    cat("\n")
    # put table into SPSS format
    spss <- toSPSS(x, statistics = "frequencies", version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put test results into SPSS format
    spss <- toSPSS(x, statistics = "test", version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
  }

}


#' @rdname sign_test
#' @export

signTest <- function(data, variables, exact = FALSE) {
  .Deprecated("sign_test")
  sign_test(data, variables = variables, exact = exact)
}
