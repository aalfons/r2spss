# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Convert R Objects to SPSS-Style Tables
#'
#' Generic function to convert an \R object into an object that contains all
#' necessary information for printing a LaTeX table that mimics the look of
#' SPSS output.
#'
#' @param object  an \R object for which a \code{to_SPSS} method exists, such
#' as objects returned by functions in \pkg{r2spss}.
#' @param \dots  additional arguments passed down to methods.
#'
#' @return
#' In order to work as expected, methods of \code{to_SPSS} should return an
#' object of class \code{"SPSS_table"}.  It should include a component
#' \code{table} that contains a data frame, which can be supplied as the
#' first argument to \code{\link{to_latex}} to print a LaTeX table that mimics
#' the look of SPSS output.  Additional components of the returned object
#' define additional arguments to be passed to the \code{"data.frame"} method
#' of \code{\link{to_latex}}.
#'
#' @author Andreas Alfons
#'
#' @note \code{to_spss} is a simple wrapper for \code{to_SPSS}, which exists
#' for convenience.
#'
#' @examples
#' # load data
#' data("Eredivisie")
#'
#' # compute a Kruskual-Wallis test to investigate whether
#' # market values differ by playing position
#' kw <- kruskal_test(Eredivisie, "MarketValue",
#'                   group = "Position")
#'
#' # convert to an object of class "SPSS_table" that
#' # contains the table with the test results
#' kw_spss <- to_SPSS(kw, statistics = "test")
#' kw_spss
#'
#' # blank out the number of degrees of freedom to ask
#' # an assignment question about it
#' kw_spss$table[2, 1] <- "???"
#'
#' # print the LaTeX table to be included in the assignment
#' to_latex(kw_spss)
#'
#' @keywords manip
#'
#' @export

to_SPSS <- function(object, ...) UseMethod("to_SPSS")


#' @rdname to_SPSS
#' @export

to_spss <- function(object, ...) to_SPSS(object, ...)


#' @export
print.SPSS_table <- function(x, ...) print(unclass(x), ...)
