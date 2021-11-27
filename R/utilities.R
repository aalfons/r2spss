# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


## function to create a \begin{tabular} statement that also defines the
## appearance with respect alignment, colors and borders
# info ........ number of columns containing information describing the
#               results.  Those will receive text in color 'blueSPSS' and
#               background in color 'graySPSS'.  By default, there will not be
#               a border before the first column or between these columns, nor
#               with the first column containing the actual results.
# results ..... number of columns containing the actual results.  Those will
#               receive background in color in color 'lightgraySPSS'.  By
#               default, there will be lines in between those columns in color
#               'darkgraySPSS', but not after the last column..
# alignment ... character vector containing an alignment specifier for each
#               column.  The default is "l" (left-aligned) for the columns with
#               information and "r" (right-aligned) for the columns with
#               results.
# border ...... logical vector indicating which borders to draw.  Its length
#               should be the number of columns plus 1.  See the descriptions
#               above for the defaults.
latexTabular <- function(info = 1, results = 1, alignment = NULL,
                         border = NULL) {
  # default justification symbols and column separator symbols
  if (is.null(alignment)) alignment <- rep.int(c("l", "r"), c(info, results))
  if (is.null(border)) {
    border <- c(rep.int(FALSE, info+1), rep.int(TRUE, results-1), FALSE)
  }
  border <- ifelse(border, "!{\\color{darkgraySPSS}\\vrule}", "")
  # prefix to define tabular environment and postfix to finalize line
  prefix <- "\\begin{tabular}{"
  postfix <- "}"
  # define font and background colors for the different columns
  font <- rep.int(c(">{\\color{blueSPSS}}", ""), c(info, results))
  background <- rep.int(c("graySPSS", "lightgraySPSS"), c(info, results))
  background <- paste0(">{\\columncolor{", background, "}}")
  # full specifications per column except left border of first column
  specs <- paste0(font, background, alignment, border[-1])
  # create \begin{tabular} statement
  paste0(prefix, border[1], paste0(specs, collapse = ""), postfix)
}


# function to create a \multicolumn statement that also defines the appearance
# with respect to alignment and borders
# text ........ character string giving the text to be written inside the
#               merged cell.  For a non-empty string, the text will receive
#               color 'blueSPSS'.
# columns ..... number of subsequent columns to merge.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "c" for centered.
# left ........ logical vector indicating whether to draw a left border in
#               color 'darkgraySPSS'.  The default is FALSE.
# right ....... logical vector indicating whether to draw a right border in
#               color 'darkgraySPSS'.  The default is FALSE.
.latexMulticolumn <- function(text, columns = 1, alignment = "c",
                              left = FALSE, right = FALSE) {
  # if requested, define left and right borders
  left <- if (left) "!{\\color{darkgraySPSS}\\vrule}" else ""
  right <- if (right) "!{\\color{darkgraySPSS}\\vrule}" else ""
  # define text color
  color <- if (text == "") "" else ">{\\color{blueSPSS}}"
  # full specification of the merged cell
  spec <- paste0(left, color, alignment, right)
  # create \multicolumn statement
  sprintf("\\multicolumn{%d}{%s}{%s}", columns, spec, text)
}
