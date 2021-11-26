# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


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
latexMulticolumn <- function(text, columns = 1, alignment = "c",
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
