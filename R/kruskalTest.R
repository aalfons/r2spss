# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Kruskal-Wallis Test
#'
#' Perform a Kruskal-Wallis test on variables of a data set.  The output is
#' printed as a LaTeX table that mimics the look of SPSS output.
#'
#' The \code{print} method first calls the \code{toSPSS} method followed by
#' \code{\link[=toLatex.SPSSTable]{toLatex}}.  Further customization can be
#' done by calling those two functions separately, and modifying the object
#' returned by \code{toSPSS}.
#'
#' @param data  a data frame containing the variables.
#' @param variable  a character string specifying the numeric variable of
#' interest.
#' @param group  a character string specifying a grouping variable.
#' @param object,x  an object of class \code{"kruskalTestSPSS"} as returned by
#' function \code{kruskalTest}.
#' @param statistics  a character string or vector specifying which SPSS tables
#' to produce.  Available options are \code{"ranks"} for a summary of the ranks
#' and \code{"test"} for test results.  For the \code{toSPSS} method, only one
#' option is allowed (the default is the table of test results), but the
#' \code{print} method allows several options (the default is to print all
#' tables).
#' @param version  a character string specifying whether the table should
#' mimic the content and look of recent SPSS versions (\code{"modern"}) or
#' older versions (<24; \code{"legacy"}).  The main differences in terms of
#' content are the label of the test statistic and that small p-values are
#' displayed differently.
#' @param digits  for the \code{toSPSS} method, an integer giving the number of
#' digits after the comma to be printed in the SPSS table.  For the
#' \code{print} method, this should be an integer vector of length 2, with the
#' first element corresponding to the number of digits in table with the
#' summary of the ranks, and the second element corresponding to the number of
#' digits in the table for the test.
#' @param \dots additional arguments to be passed down to
#' \code{\link{formatSPSS}}.
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


#' @rdname kruskalTest
#' @export

toSPSS.kruskalTestSPSS <- function(object, statistics = c("test", "ranks"),
                                   version = r2spssOptions$get("version"),
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
    version <- match.arg(version, choices = getVersionValues())
    legacy <- version == "legacy"
    # extract results
    # put test results into SPSS format
    rn <- c(if (legacy) "Chi-Square" else "Kruskal-Wallis H",
            "df", "Asymp. Sig.")
    values <- unlist(object$test)
    # format results nicely
    args <- list(values, digits = digits, ...)
    if (is.null(args$pValue)) args$pValue <- c(FALSE, FALSE, !legacy)
    if (is.null(args$checkInt)) args$checkInt <- c(FALSE, TRUE, FALSE)
    formatted <- do.call(formatSPSS, args)
    # put test results into SPSS format
    test <- data.frame(formatted, row.names = rn)
    names(test) <- object$variable
    # define footnotes
    footnotes <- c("Kruskal Wallis Test",
                   paste("Grouping Variable:", object$group))
    footnotes <- data.frame(marker = c("a", "b"), row = rep.int("main", 2),
                            column = rep.int(NA_integer_, 2),
                            text = footnotes)
    # construct list containing all necessary information
    spss <- list(table = test, main = "Test Statistics",
                 header = TRUE, rowNames = TRUE, info = 0,
                 footnotes = footnotes, version = version)

  } else stop ("type of 'statistics' not supported")  # shouldn't happen

  # add class and return object
  class(spss) <- "SPSSTable"
  spss

}


#' @rdname kruskalTest
#' @export

print.kruskalTestSPSS <- function(x, statistics = c("ranks", "test"),
                                  version = r2spssOptions$get("version"),
                                  digits = 2:3, ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  version <- match.arg(version, choices = getVersionValues())
  digits <- rep_len(digits, 2)

  ## print LaTeX table for ranks
  if ("ranks" %in% statistics) {
    cat("\n")
    # put table into SPSS format
    spss <- toSPSS(x, digits = digits[1], statistics = "ranks",
                   version = version, ...)
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
    spss <- toSPSS(x, digits = digits[2], statistics = "test",
                   version = version, ...)
    # print LaTeX table
    toLatex(spss, version = version)
    cat("\n")
  }

}
