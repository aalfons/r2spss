# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Descriptive Statistics
#'
#' Compute descriptive statistics of numeric variables of a data set (number of
#' observations, minimum, maximum, mean, standard deviaiton).  The output is
#' printed as a LaTeX table that mimics the look of SPSS output.
#'
#' The \code{print} method first calls the \code{to_SPSS} method followed
#' by \code{\link{to_latex}}.  Further customization can be done by calling
#' those two functions separately, and modifying the object returned by
#' \code{to_SPSS}.
#'
#' @param data  a data frame containing the variables.
#' @param variables  a character vector specifying numeric variables for which
#' to compute descriptive statistics.
#' @param object,x  an object of class \code{"descriptives_SPSS"} as returned
#' by function \code{descriptives}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the SPSS table.
#' @param \dots additional arguments to be passed down to
#' \code{\link{format_SPSS}}.
#' @param version  a character string specifying whether the table should
#' mimic the look of recent SPSS versions (\code{"modern"}) or older versions
#' (<24; \code{"legacy"}).
#'
#' @return
#' An object of class \code{"descriptives_SPSS"} with the following components:
#' \describe{
#'   \item{\code{classes}}{a character vector giving the (first) class of the
#'   variables of interest.}
#'   \item{\code{descriptives}}{a data frame containing the descriptive
#'   statistics.}
#'   \item{\code{n}}{an integer giving the number of observations.}
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
  out <- list(classes = classes, descriptives = desc, n = n)
  class(out) <- "descriptives_SPSS"
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
  data.frame(N = n, Minimum = range[1], Maximum = range[2], Mean = mean,
             "Std. Deviation" = sd, check.names = FALSE)
}


#' @rdname descriptives
#' @export

to_SPSS.descriptives_SPSS <- function(object, digits = 2, ...) {
  # put table of results into SPSS format
  p <- ncol(object$descriptives)
  descriptives <- rbind(object$descriptives,
                        "Valid N (listwise)" = c(object$n, rep.int(NA, p-1)))
  # define header with line breaks
  col_names <- names(descriptives)
  header <- c("", wrap_text(col_names, limit = 10))
  # format table nicely
  args <- list(descriptives, digits = digits, ...)
  if (is.null(args$check_int)) {
    args$check_int <- col_names %in% c("Minimum", "Maximum")
  }
  formatted <- do.call(format_SPSS, args)
  # define grid lines
  minor <- seq_len(nrow(formatted) - 1)
  # construct return object
  spss <- list(table = formatted, main = "Descriptive Statistics",
               header = header, row_names = TRUE, info = 0,
               minor = minor)
  class(spss) <- "SPSS_table"
  spss
}


#' @rdname descriptives
#' @export

print.descriptives_SPSS <- function(x, version = r2spss_options$get("version"),
                                    ...) {
  # initializations
  version <- match.arg(version, choices = get_version_values())
  # put table of results into SPSS format
  spss <- to_SPSS(x, ...)
  # print LaTeX table
  to_latex(spss, version = version)
}
