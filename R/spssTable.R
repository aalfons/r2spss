# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


## header ... can also be character vector with different names or a list that
##            defines the hierarchy, e.g.:
##            list("Mean", "Std. Error",
##                 "Confidence Interval" = c("Lower\nBound", "Upper\nBound"))
##            Line separators break up the header into different lines

spssTable <- function(object, ...) UseMethod("spssTable")

spssTable.data.frame <- function(object, main = NULL, sub = NULL,
                                 header = TRUE, label = NULL,
                                 row.names = TRUE, info = NULL,
                                 digits = 3, alignment = NULL,
                                 border = NULL, footnotes = NULL,
                                 theme = c("modern", "legacy"),
                                 ...) {

  ## TODO: Add argument append, which if TRUE sets writing titles, header, and
  ##       footnotes to FALSE, and also suppressed the \begin{tabulate} and
  ##       \end{tabulate} statements.  But then it's unclear where and how to
  ##       begin and end the tabulate environment.
  ##
  ## It's better to have an argument 'environment' that can take the values
  ## TRUE, FALSE, "begin", and "end".  Alignment specifiers and border
  ## indicators should only be checked if this is TRUE or "begin", or if
  ## headers are requested.

  ## initializations
  # check main and sub title
  writeMain <- !is.null(main)
  if (writeMain && (!is.character(main) || length(main) != 1)) {
    stop("'main' must be a single character string")
  }
  writeSub <- !is.null(sub)
  if (writeSub && (!is.character(sub) || length(sub) != 1)) {
    stop("'sub' must be a single character string")
  }
  # check header
  if (is.logical(header)) {
    writeHeader <- isTRUE(header)
    header <- names(object)
  } else {
    writeHeader <- TRUE
    if (is.list(header)) stop("not implemented yet")
    else if (is.character(header)) stop("not implemented yet")
    else stop("'header' must be a logical, a list, or a character vector")
  }
  # check whether a label should be written
  addLabel <- !is.null(label)
  if (addLabel && (!is.character(label) || length(label) != 1)) {
    stop("'label' must be a single character string")
  }
  # check whether row names should be written
  row.names <- isTRUE(row.names)
  # check columns that contain auxiliary information
  if (is.null(info)) info <- if (row.names) 0 else 1
  info <- info + addLabel + row.names
  # compute total number of columns
  d <- dim(object)
  columns <- d[2] + addLabel + row.names
  if (columns == 0) stop("table to be written must have at least one column")
  # compute number of columns actually containing results
  results <- columns - info
  # check column alignment specifiers
  if (is.null(alignment)) {
    alignment <- list(header = rep.int(c("l", "c"), c(info, results)),
                      table = rep.int(c("l", "r"), c(info, results)))
  }
  # TODO: perform checks
  # check theme
  theme <- match.arg(theme)
  legacy <- theme == "legacy"
  # check border indicators
  if (is.null(border)) {
    if (legacy) {
      if (info <= 1) border <- rep.int(TRUE, columns+1)
      else border <- c(TRUE, rep.int(FALSE, info-1), rep.int(TRUE, results+1))
    } else border <- c(rep.int(FALSE, info+1), rep.int(TRUE, results-1), FALSE)
  }
  # check whether footnotes should be written
  # TODO: this could also be a list with one component that indicates the
  # position of the footnote marker (e.g, "main", "sub", or an index pair) and
  # another component that is a character vector containing the footnotes
  writeFootnotes <- !is.null(footnotes)
  if (writeFootnotes && (!is.character(footnotes) || length(footnotes) == 0)) {
    stop("'footnotes' must be a character vector of nonzero length")
  }

  ## write \begin{tabular} statement
  cat(latexTabular(columns, info, alignment = alignment$table,
                   border = border, theme = theme))
  cat("\n")

  ## if supplied, write main and sub title
  if (writeMain) {
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{", columns, "}{c}{\\textbf{", main, "}} \\\\\n", sep="")
  }
  if (writeSub) {
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{", columns, "}{l}{", sub, "} \\\\\n", sep="")
  }

  ## if supplied, write table header
  if (writeHeader) {
    if (legacy) cat("\\noalign{\\smallskip}\\hline\n")
    else cat("\\noalign{\\smallskip}\n")
    # TODO: For now, only a simple header layout with one row of cells is
    #       supported.  In addition, only row names are supported, but not
    #       yet an additional label.
    headerCells <- mapply(latexHeaderCell, text = c(if (row.names) "", header),
                          columns = c(if (row.names) 1, rep.int(1, d[2])),
                          alignment = alignment$header,
                          left = c(border[1], rep.int(FALSE, columns-1)),
                          right = border[-1], MoreArgs = list(theme = theme),
                          USE.NAMES = FALSE)
    cat(paste(headerCells, collapse = " & "), "\\\\\n")
    cat("\\hline\n")
  }

  ## format and write table
  # format everything nicely
  if (legacy) {
    formatted <- formatSPSS(object, digits = digits, pValue = FALSE)
  } else formatted <- formatSPSS(object, digits = digits)
  # write table
  # (note that the formatted object is a matrix, but it has row names since the
  # original object is a data.frame, which always have row names)
  for (rn in rownames(formatted)) {
    cat(if (row.names) paste(rn, "&"),
        paste0(formatted[rn, ], collapse = " & "),
        "\\\\\n")
  }

  ## TODO: if supplied, write footnotes

  ## finalize table
  cat("\\hline\\noalign{\\smallskip}\n")
  cat("\\end{tabular}\n")

}




## function to create a \begin{tabular} statement that also defines the
## appearance with respect alignment, colors and borders
# columns ..... total number of columns.
# info ........ number of columns containing auxiliary information on the
#               results.
# alignment ... character vector containing an alignment specifier for each
#               column.  The default is "l" (left-aligned) for the columns with
#               auxiliary information and "r" (right-aligned) for the remaining
#               columns results.
# border ...... logical vector indicating which borders to draw.  Its length
#               should be columns + 1.  See the functions below for the
#               defaults.
# theme ....... character string specifying whether the table should have the
#               appearance of recent SPSS version ("modern") or older ones
#               ("legacy").

latexTabular <- function(columns, info = 1, alignment, border,
                         theme = "modern") {
  if (theme == "legacy") legacyTabular(columns, info, alignment, border)
  else modernTabular(columns, info, alignment, border)
}


# Columns with auxiliary information on the results will receive text in color
# 'blueSPSS' and background in color 'graySPSS'.  By default, there will not be
# a border before the first column or between these columns, nor with the first
# remaining column.  The remaining columns will receive background in color in
# color 'lightgraySPSS'.  By default, there will be lines in between those
# columns in color 'darkgraySPSS', but not after the last column.

modernTabular <- function(columns, info = 1, alignment, border) {
  # number columns containing the actual results
  results <- columns - info
  # specify borders
  border <- ifelse(border, "!{\\color{darkgraySPSS}\\vrule}", "")
  # define font and background colors for the different columns
  font <- rep.int(c(">{\\color{blueSPSS}}", ""), c(info, results))
  background <- rep.int(c("graySPSS", "lightgraySPSS"), c(info, results))
  background <- paste0(">{\\columncolor{", background, "}}")
  # full specifications per column except left border of first column
  specs <- paste0(font, background, alignment, border[-1])
  # create \begin{tabular} statement
  paste0("\\begin{tabular}{", border[1], paste0(specs, collapse = ""), "}")
}

# Columns with auxiliary information on the results will receive a border
# before the first column, but not in between these columns.  There will be
# border with the first remaining column, as well as between the remaining
# columns and after the last column.

legacyTabular <- function(columns, info = 1, alignment, border) {
  # number columns containing the actual results
  results <- columns - info
  # specify borders
  border <- ifelse(border, "|", "")
  # full specifications per column except left border of first column
  specs <- paste0(alignment, border[-1])
  # create \begin{tabular} statement
  paste0("\\begin{tabular}{", border[1], paste0(specs, collapse = ""), "}")
}


# function to create a header cell, which in many cases contains a
# \multicolumn statement that also defines the appearance with respect
# to alignment and borders
# text ........ character string giving the text to be written inside the
#               merged cell.
# columns ..... number of subsequent columns to merge.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "c" for centered.
# left ........ logical vector indicating whether to draw a left border.
# right ....... logical vector indicating whether to draw a right border.
# theme ....... character string specifying whether the cell should have the
#               appearance of recent SPSS version ("modern") or older ones
#               ("legacy").

latexHeaderCell <- function(text = "", columns = 1, alignment = "c",
                            left = FALSE, right = FALSE, theme = "modern") {
  if (theme == "legacy") {
    legacyHeaderCell(text, columns, alignment, left, right)
  } else modernHeaderCell(text, columns, alignment, left, right)
}


# for a non-empty string, the text will receive color 'blueSPSS', and borders
# will be drawn in color 'darkgraySPSS'
modernHeaderCell <- function(text = "", columns = 1, alignment = "c",
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


# for a single cell with an empty string, no formatting is necessary, otherwise
# text and colors will get the default color (typically black)
legacyHeaderCell <- function(text = "", columns = 1, alignment = "c",
                             left = FALSE, right = FALSE) {
  # nothing to do for a single cell with an empty string
  if (text == "" && columns == 1) return("")
  # if requested, define left and right borders
  left <- if (left) "|" else ""
  right <- if (right) "|" else ""
  # full specification of the merged cell
  spec <- paste0(left, alignment, right)
  # create \multicolumn statement
  sprintf("\\multicolumn{%d}{%s}{%s}", columns, spec, text)
}
