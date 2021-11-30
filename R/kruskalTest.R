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
#'   \item{\code{variable}}{a character string containing the name of the
#'   numeric variable of interest.}
#'   \item{\code{group}}{a character string containing the name of the
#'   grouping variable.}
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
#' # test whether market values differ by playing position
#' kruskalTest(Eredivisie, "MarketValue", group = "Position")
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


## convert R results to all necessary information for SPSS-like table
#' @export

toSPSS.kruskalTestSPSS <- function(object, statistics = c("test", "ranks"),
                                   version = c("modern", "legacy"),
                                   digits = NULL, ...) {

  ## initializations
  statistics <- match.arg(statistics)

  ## put requested results into SPSS format
  if (statistics == "ranks") {
    # initializations
    if (is.null(digits)) digits <- 2
    # put table into SPSS format
    p <- ncol(object$statistics)
    N <- sum(object$statistics$N)
    ranks <- rbind(object$statistics, Total = c(N, rep.int(NA, p)))
    # format table nicely
    formatted <- formatSPSS(ranks, digits = digits, ...)
    # define header
    header <- c("", object$group, names(ranks))
    # construct list containing all necessary information
    spss <- list(table = formatted, main = "Ranks", header = header,
                 label = object$variable, rowNames = TRUE, info = 0)
  } else if (statistics == "test") {
    # initializations
    if (is.null(digits)) digits <- 3
    version <- match.arg(version)
    legacy <- version == "legacy"

    # put test results into SPSS format
    rn <- c(if (legacy) "Chi-Square" else "Kruskal-Wallis H",
            "df", "Asymp. Sig.")
    test <- data.frame(unlist(object$test), row.names = rn)
    names(test) <- object$variable
    # format table nicely
    args <- list(test, digits = digits, ...)
    if (is.null(args$pValue)) args$pValue <- !legacy
    if (is.null(args$checkInt)) args$checkInt <- TRUE
    formatted <- do.call(formatSPSS, args)
    # define footnotes
    footnotes <- c("Kruskal Wallis Test",
                   paste("Grouping Variable:", object$group))
    footnotes <- data.frame(marker = c("a", "b"), row = rep.int("main", 2),
                            column = rep.int(NA_integer_, 2),
                            text = footnotes)
    # construct list containing all necessary information
    spss <- list(table = formatted, main = "Test Statistics",
                 header = TRUE, rowNames = TRUE, info = 0,
                 footnotes = footnotes, version = version)
  } else stop ("type of 'statistics' not supported")  # shouldn't happen

  # add class and return object
  class(spss) <- "SPSSTable"
  spss

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

print.kruskalTestSPSS <- function(x, statistics = c("ranks", "test"),
                                  theme = c("modern", "legacy"),
                                  digits = 2:3, ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)
  theme <- match.arg(theme)
  legacy <- theme == "legacy"

  ## print LaTeX table for ranks
  if ("ranks" %in% statistics) {
    cat("\n")
    # put table into SPSS format
    spss <- toSPSS(x, digits = digits[1], statistics = "ranks",
                   version = theme, ...)
    # print LaTeX table
    toLatex(spss, theme = theme)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put test results into SPSS format
    spss <- toSPSS(x, digits = digits[2], statistics = "test",
                   version = theme, ...)
    # print LaTeX table
    toLatex(spss, theme = theme)
    cat("\n")
  }
}
