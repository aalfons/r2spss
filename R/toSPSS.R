# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


## generic function to convert R objects into an object of class
## "setupTableSPSS", which contains all the necessary information for printing
## the specific table in the style of SPSS output.  This allows for easy
## customization of the SPSS-like output, e.g., to blank out information on the
## degrees of freedom for use in an exam question.

#' @export
toSPSS <- function(object, ...) UseMethod("toSPSS")


#' @export
print.SPSSTable <- function(x, ...) print(unclass(x), ...)


#' @export
toLatex.SPSSTable <- function(object, theme = c("modern", "legacy"), ...) {
  # object of class "SPSSTable" contains all the relevant information that
  # needs to be passed down to the workhorse method
  args <- object
  # first argument needs to be renamed to be in line with function definition
  rename <- names(args) == "table"
  names(args)[rename] <- "object"
  # For some methods, the information in the table has changed as well for
  # newer versions of SPSS, and not just the appearance of the table.  In that
  # case, the appearance needs to match the version of the table.  Otherwise,
  # the appearance is defined by argument 'theme'.
  which <- grep("version", names(args), fixed = TRUE)
  if (length(which) == 0) args$theme <- match.arg(theme)
  else names(args)[which] <- "theme"
  # call workhorse method
  do.call(toLatex, args)
}
