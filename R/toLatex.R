# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


#' Print LaTeX tables that mimic the look of SPSS output
#'
#' Use information from an \R object to print a LaTeX table that mimics the
#' look of SPSS output.  Typically, one would first call \code{\link{toSPSS}}
#' with an object returned by a function in \pkg{r2spss}, and then call
#' \code{toLatex} with the resulting object of class \code{"SPSSTable"} to
#' print the LaTeX table.  Note that the \code{print} methods in \pkg{r2spss}
#' perform these two steps at once, but calling \code{\link{toSPSS}} and
#' \code{toLatex} separately can be useful for customization of the LaTeX
#' table.
#'
#' The \code{"SPSSTable"} method takes component \code{table} of the object and
#' supplies it to the \code{data.frame} method, with additional components in
#' the object being passed as additional arguments.
#'
#' The \code{"data.frame"} method allows to extend the functionality of
#' \pkg{r2spss} with additional LaTeX tables that mimic the look of SPSS
#' output.
#'
#' @param object  an object of class \code{"SPSSTable"} as returned by
#' \code{toSPSS} methods, or a \code{data.frame}.
#' @param main  a single character string defining the main title of the SPSS
#' table, or \code{NULL} to suppress the main title.
#' @param sub  a single character string defining the sub-title of the SPSS
#' table, or \code{NULL} to suppress the sub-title.
#' @param header  a logical indicating whether to include a header in the SPSS
#' table based on the column names of \code{object} (defaults to \code{TRUE}).
#' Alternatively, it is possible to supply a character vector giving the
#' header of each column, or a list defining a complex header layout with
#' merged header cells.  In the latter case, the list can have up to three
#' components, with each component defining one level of the header.  The last
#' list component should be a character vector giving the bottom-level header
#' of each column.  The other list components should be data frames with the
#' following columns:
#' \describe{
#'   \item{\code{first}}{an integer vecot giving the first column of each
#'   (merged) header cell.}
#'   \item{\code{last}}{an integer vector giving the last column of each
#'   (merged) header cell.}
#'   \item{\code{text}}{a character vector containing the text of each (merged)
#'   header cell.}
#' }
#' Line breaks (character \code{\\n}) can be included to wrap the text of a
#' header cell over several rows.
#' @param label  a character string giving a label to be added as the first
#' column of the table, or \code{NULL} to suppress such a column.  In many
#' SPSS tables, this contains the name of a variable used in the analysis.
#' @param rowNames  a logical indicating whether to add the row names of
#' \code{object} as a column in the SPSS table (defaults to \code{TRUE}).
#' Alternatively, it is possible to supply a character vector giving the
#' row labels to be added as a column.  Line breaks (character \code{\\n}) can
#' be included to wrap the text of a row label over several rows.
#' @param info  an integer giving the number of columns in the SPSS table that
#' contain auxiliary information on the results.  This has an effect of the
#' default formatting, alignment, and borders.  The default is 0 if
#' \code{rowNames} is \code{FALSE} and 1 otherwise.  Note that a column defined
#' by \code{label} and a column defined by \code{rowNames} are always added to
#' \code{info} if the former are supplied.
#' @param alignment  a list with components \code{header} and \code{table},
#' with each component being a character vector that contains the LaTeX
#' alignment specifiers of the header and table body, respectively, of each
#' column.  Permissible alignment specifiers are \code{"l"} for left-aligned,
#' \code{"c"} for centered, and \code{"r"} for right aligned.  The default is
#' left-aligned for the header and table body of the columns containing
#' auxiliary information, and centered and right-aligned, respectively, for the
#' header and table body of the columns containing the actual results.  It
#' should not be necessary to set the column alignment manually.
#' @param border  a logical vector indicating which (outer and inner) vertical
#' borders should be drawn.  The default is that tables that mimic recent
#' versions of SPSS (\code{version = "modern"}) draw only borders in between
#' columns that contain the actual results, whereas tables that mimic older
#' versions of SPSS (\code{version = "legacy"}) draw all borders except in
#' between columns containing auxiliary information.  It should not be
#' necessary to set the vertical borders manually.
#' @param footnotes  a character vector giving footnotes to be added below the
#' SPSS table, or \code{NULL} to suppress footnotes.  Alternatively, it is
#' possible to supply a data frame with the following columns:
#' \describe{
#'   \item{\code{marker}}{character vector giving footnote markers to be
#'   included in a cell of the SPSS table.  For footnotes without a marker,
#'   an empty character string can be used.}
#'   \item{\code{row}}{an integer vector specifying the row of the SPSS table
#'   in which to include each footnote marker, or \code{NA} for footnotes
#'   without a marker.  In addition, the character strings \code{"main"} and
#'   \code{"sub"} can be used to include footnote markers in the main title
#'   and sub-title, respectively.}
#'   \item{\code{column}}{an integer vector specifying the column of the SPSS
#'   table in which to include each footnote marker, or \code{NA} for footnotes
#'   without a marker or footnote markers in the main title or sub-title.}
#'   \item{\code{text}}{a character vector containing the text of each
#'   footnote.}
#' }
#' @param major  an integer vector specifying the rows of the SPSS table after
#' which to draw major grid lines (which always stretch across all columns o
#' the table), or \code{NULL} to suppress any major grid lines.  This is only
#' relevant for drawing lines in between rows of the table body.  Horizontal
#' table borders are always drawn.
#' @param minor  an integer vector specifying the rows of the SPSS table after
#' which to draw minor grid lines that stretch across all columns of the table,
#' or \code{NULL} to suppress any minor grid lines.  Alternatively, this can
#' be a data frame with the following columns defining partial lines:
#' \describe{
#'   \item{\code{row}}{an integer vector specifying the rows of the SPSS table
#'   after which to draw minor grid lines.}
#'   \item{\code{first}}{an integer vector specifying the first column of each
#'   partial line.}
#'   \item{\code{last}}{an integer vector specifying the last column of each
#'   partial line.}
#' }
#' @param version  a character string specifying whether the table should
#' mimic the look of recent SPSS versions (\code{"modern"}) or older versions
#' (<24; \code{"legacy"}).  For the \code{"SPSSTable"} method, note that also
#' the \emph{content} of some tables generated by functions in \pkg{r2spss} is
#' different for current and older SPSS versions.  These objects contain a
#' component \code{"version"} which will passed to the \code{"data.frame"}
#' method to ensure that the content and look of the table match.  Other
#' tables have the same content irrespective of the SPSS version, and this
#' argument controls the look of those tables.
#' @param \dots  for the \code{"data.frame"} method, additional arguments to be
#' passed to \code{\link{formatSPSS}}.  For the \code{"SPSSTable"} method,
#' additional arguments are currently ignored.
#'
#' @return  Nothing is returned, the function is called for its side effects.
#'
#' @note
#' LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
#' may require several LaTeX compilations to be displayed correctly.
#'
#' @author Andreas Alfons
#'
#' @examples
#' ## Kruskal-Wallis test example
#'
#' # load data
#' data("Eredivisie")
#'
#' # compute a Kruskual-Wallis test to investigate whether
#' # market values differ by playing position
#' kw <- kruskalTest(Eredivisie, "MarketValue",
#'                   group = "Position")
#'
#' # convert to an object of class "SPSSTable" that
#' # contains the table with the test results
#' kwSPSS <- toSPSS(kw, statistics = "test")
#' kwSPSS
#'
#' # blank out the number of degrees of freedom to ask
#' # an assignment question about it
#' kwSPSS$table[2, 1] <- "???"
#'
#' # print the LaTeX table to be included in the assignment
#' toLatex(kwSPSS)
#'
#'
#' ## t test example
#'
#' # load data
#' data("Exams")
#'
#' # test whether the average grade on the resit
#' # differs from 5.5 (minimum passing grade)
#' t <- tTest(Exams, "Resit", mu = 5.5)
#'
#' # convert to an object of class "SPSSTable" that
#' # contains the table with the test results
#' tSPSS <- toSPSS(t, statistics = "test")
#'
#' # this is an example of a complex header layout
#' tSPSS$header
#'
#' # add additional line breaks in bottom-level header
#' tSPSS$header[[3]] <- gsub("-", "-\n", tSPSS$header[[3]],
#'                           fixed = TRUE)
#'
#' # print the LaTeX table
#' toLatex(tSPSS)
#'
#' @keywords print
#'
#' @importFrom utils toLatex
#' @export

toLatex.SPSSTable <- function(object, version = c("modern", "legacy"), ...) {
  # object of class "SPSSTable" contains all the relevant information that
  # needs to be passed down to the data.frame method
  args <- object
  # first argument needs to be renamed to be in line with function definition
  rename <- names(args) == "table"
  names(args)[rename] <- "object"
  # For some methods, the information in the table has changed as well for
  # newer versions of SPSS, and not just the appearance of the table.  In that
  # case, the appearance needs to match the version of the table.  Otherwise,
  # the appearance is defined by argument 'version'.
  which <- grep("version", names(args), fixed = TRUE)
  if (length(which) == 0) args$version <- match.arg(version)
  # call workhorse method
  do.call(toLatex, args)
}


#' @rdname toLatex.SPSSTable
#' @export

toLatex.data.frame <- function(object, main = NULL, sub = NULL, header = TRUE,
                               label = NULL, rowNames = TRUE, info = NULL,
                               alignment = NULL, border = NULL,
                               footnotes = NULL, major = NULL, minor = NULL,
                               version = c("modern", "legacy"), ...) {

  ## initializations
  d <- dim(object)
  if (d[1] == 0) stop("table to be written has no rows")
  # check main and sub title
  writeMain <- !is.null(main)
  if (writeMain && (!is.character(main) || length(main) != 1)) {
    stop("'main' must be a single character string")
  }
  writeSub <- !is.null(sub)
  if (writeSub && (!is.character(sub) || length(sub) != 1)) {
    stop("'sub' must be a single character string")
  }
  # check whether a label should be written
  addLabel <- !is.null(label)
  if (addLabel && (!is.character(label) || length(label) != 1)) {
    stop("'label' must be a single character string")
  }
  # check whether row names should be written
  if (is.logical(rowNames)) {
    addRowNames <- isTRUE(rowNames)
    rowNames <- row.names(object)
  } else {
    addRowNames <- TRUE
    if (is.character(rowNames)) {
      addRowNames <- TRUE
      if (length(rowNames) != d[1]) {
        stop(sprintf("'rowNames' must have length %d", d[1]))
      }
    } else stop("'rowNames' must be TRUE/FALSE or a character vector")
  }
  # check columns that contain auxiliary information
  if (is.null(info)) info <- if (addRowNames) 0 else 1
  info <- info + addLabel + addRowNames
  # compute total number of columns
  columns <- d[2] + addLabel + addRowNames
  if (columns == 0) stop("table to be written must have at least one column")
  # check header
  if (is.logical(header)) {
    writeHeader <- isTRUE(header)
    header <- c(if(addLabel) "", if (addRowNames) "", names(object))
  } else {
    writeHeader <- TRUE
    if (is.character(header)) {
      if (length(header) != columns) {
        stop(sprintf("'header' must have length %d", columns))
      }
    } else if (!is.list(header)) {
      stop("'header' must be TRUE/FALSE, a character vector, or a list")
    }
  }
  # compute number of columns actually containing results
  results <- columns - info
  # check column alignment specifiers
  if (is.null(alignment)) {
    alignment <- list(header = rep.int(c("l", "c"), c(info, results)),
                      table = rep.int(c("l", "r"), c(info, results)))
  } else {
    # TODO: perform checks
  }
  # check theme
  version <- match.arg(version)
  legacy <- version == "legacy"
  # check border indicators
  if (is.null(border)) {
    if (legacy) {
      if (info <= 1) border <- rep.int(TRUE, columns+1)
      else border <- c(TRUE, rep.int(FALSE, info-1), rep.int(TRUE, results+1))
    } else border <- c(rep.int(FALSE, info+1), rep.int(TRUE, results-1), FALSE)
  }
  # check whether footnotes should be written
  writeFootnotes <- !is.null(footnotes)
  if (writeFootnotes) {
    if (is.data.frame(footnotes)) {
      targetNames <- c("marker", "row", "column", "text")
      if (!all(targetNames %in% names(footnotes))) {
        stop("footnotes' must have columns ",
             paste(paste0("'", targetNames, "'"), collapse = ", "))
      }
      # reorder footnotes according to markers
      # (and remove footnotes whose marker is NA)
      footnotes <- footnotes[order(footnotes$marker, na.last = NA), ]
      # parse footnote markers to check where they need to be inserted into
      # table (if any)
      insertFootnotes <- parseFootnoteMarkers(footnotes$row, footnotes$column,
                                              marker = footnotes$marker)
    } else {
      insertFootnotes <- list()  # no footnote markers in table
      if (!is.character(footnotes)) {
        stop("'footnotes' must be a character vector or data.frame")
      }
    }
  }
  # check major grid lines
  drawMajor <- !is.null(major)
  if (drawMajor) {
    if (!is.numeric(major)) {
      stop("'major' must be an integer vector")
    }
    major <- sort(as.integer(major))
    keep <- (major > 0) & (major < d[1])
    if (!all(keep)) {
      major <- major[keep]
      warning("some indices for major grid lines are out of bounds; ",
              "those have been discarded")
    }
  }
  # check minor grid lines
  drawMinor <- !is.null(minor)
  if (drawMinor) {
    if (is.data.frame(minor)) {
      # draw only partial lines
      partialMinor <- TRUE
      # perform checks
      targetNames <- c("row", "first", "last")
      if (!all(targetNames %in% names(minor))) {
        stop("minor' must have columns ",
             paste(paste0("'", targetNames, "'"), collapse = ", "))
      }
    } else {
      # draw lines across full table
      partialMinor <- FALSE
      # perform checks
      if (!is.numeric(minor)) {
        stop("'minor' must be an integer vector or data.frame")
      }
      minor <- sort(as.integer(minor))
      keep <- (minor > 0) & (minor < d[1])
      if (!all(keep)) {
        minor <- minor[keep]
        warning("some indices for minor grid lines are out of bounds; ",
                "those have been discarded")
      }
    }
  }

  ## for modern theme, we need to process titles and header to know which
  ## cells should receive the background color

  ## if supplied, process main and sub title
  if (writeMain) {
    # if supplied, insert footnote markers
    if (writeFootnotes && !is.null(insertFootnotes$main)) {
      main <- paste0(main, insertFootnotes$main)
    }
    # get LaTeX statement(s)
    latexMain <- latexTitle(main, columns = columns, alignment = "c",
                            version = version)
  } else latexMain <- NULL
  if (writeSub) {
    # if supplied, insert footnote markers
    if (writeFootnotes && !is.null(insertFootnotes$sub)) {
      sub <- paste0(sub, insertFootnotes$sub)
    }
    # get LaTeX statement(s)
    latexSub <- latexMulticolumn(sub, columns = columns, alignment = "l",
                                 version = version)
  } else latexSub <- NULL

  ## if supplied, write table header
  if (writeHeader) {
    # parse header layout
    leftBorder <- c(border[1], rep.int(FALSE, columns-1))
    rightBorder <- border[-1]
    headerList <- parseHeaderLayout(header, alignment = alignment$header,
                                    left = leftBorder, right = rightBorder)
  } else headerList <- NULL

  ## create LaTeX table

  ## write \begin{tabular} statement
  skip <- length(latexMain) + length(latexSub) + length(headerList)
  cat(latexBeginTabular(alignment$table, border, nrow = c(skip, d[1]),
                        ncol = c(info, results), version = version),
      sep = "")

  ## if supplied, write main and sub title
  if (writeMain) {
    cat("\\noalign{\\smallskip}\n")
    cat(latexMain, sep = "")
    cat("\\noalign{\\smallskip}\n")
  }
  if (writeSub) cat(latexSub, sep = "")

  ## if supplied, write table header
  if (writeHeader) {
    # for legacy theme, draw line above header
    if (legacy) cat(latexLine())
    # loop over rows in header layout
    for (row in headerList) {
      # write current row
      headerCells <- mapply(latexHeaderCell, text = row$text,
                            columns = row$columns,
                            alignment = row$alignment,
                            left = row$left, right = row$right,
                            MoreArgs = list(version = version),
                            USE.NAMES = FALSE)
      cat(paste(headerCells, collapse = " & "), "\\\\\n")
      # for legacy theme, add partial lines under merged cells
      merged <- row$merged
      if (legacy && !is.null(merged)) {
        for (j in seq_len(nrow(merged))) {
          cat(latexPartialLine(merged[j, "first"], merged[j, "last"]))
        }
      }
    }
    # draw line to separate header from table body
    cat(latexTopLine(version = version))
  }

  ## format and write table body
  # format everything nicely if it isn't already
  ok <- vapply(object, inherits, logical(1), "character", USE.NAMES = FALSE)
  if (all(ok)) formatted <- as.matrix(object)
  else {
    if (legacy) formatted <- formatSPSS(object, ...)
    else {
      # for modern theme, set a reasonable default to format p-values nicely
      args <- list(object, ...)
      if (is.null(args$pValue)) {
        args$pValue <- grepl("Sig.", names(object), fixed = TRUE)
      }
      formatted <- do.call(formatSPSS, args)
    }
  }
  # if supplied, insert footnote markers
  if (writeFootnotes) {
    markers <- insertFootnotes$body
    if (!is.null(markers)) {
      for (i in seq_len(nrow(markers))) {
        k <- markers[i, "row"]
        l <- markers[i, "column"]
        formatted[k, l] <- paste0(formatted[k, l], markers[i, "marker"])
      }
    }
  }
  # write table body
  alignLabel <- alignment$table[1]
  alignName <- alignment$table[addLabel+1]
  for (i in seq.int(nrow(formatted))) {
    # write current row
    if (i == 1) {
      cat(if (addLabel) paste(latexInfoCell(label, alignLabel, version = version), "&"),
          if (addRowNames) paste(latexInfoCell(rowNames[i], alignName, version = version), "&"),
          paste0(formatted[i, ], collapse = " & "),
          "\\\\\n")
    } else {
      cat(if (addLabel) " &",
          if (addRowNames) paste(latexInfoCell(rowNames[i], alignName, version = version), "&"),
          paste0(formatted[i, ], collapse = " & "),
          "\\\\\n")
    }
    # if requested, draw major grid line
    if (drawMajor && (i %in% major)) cat(latexMajorLine(version = version))
    # if requested, draw minor grid line
    if (drawMinor) {
      if (partialMinor) {
        which <- match(i, minor$row)
        if (!is.na(which)) {
          cat(latexMinorLine(minor[which, "first"], minor[which, "last"],
                             version = version))
        }
      } else if (i %in% minor) cat(latexMinorLine(version = version))
    }
  }
  # draw line below table body
  cat(latexBottomLine(version = version))

  ## if supplied, write footnotes
  if (writeFootnotes) {
    if (is.data.frame(footnotes)) {
      # writing footnotes is more elaborate if markers are supplied
      for (i in seq_len(nrow(footnotes))) {
        footnote <- parseFootnoteText(footnotes$text[i], footnotes$marker[i])
        cat(latexMulticolumn(footnote, columns = columns, alignment = "l",
                             version = version))
      }
    } else if (is.character(footnotes)) {
      # simply write footnotes with \mulitcolumn statements
      for (footnote in footnotes) {
        cat(latexMulticolumn(footnote, columns = columns, alignment = "l",
                             version = version))
      }
    }
  }

  ## write \end{tabular} statement
  cat("\\noalign{\\smallskip}\n")
  cat(latexEndTabular(version = version))

}


## function to create a \begin{tabular} statement that also defines the
## appearance with respect alignment, colors and borders
# alignment ... character vector containing an alignment specifier for each
#               column.  The default is "l" (left-aligned) for the columns with
#               auxiliary information and "r" (right-aligned) for the remaining
#               columns results.
# border ...... logical vector indicating which borders to draw.  Its length
#               should be columns + 1.  See the functions below for the
#               defaults.
# nrow ........ integer vector of length two, with the first element giving
#               the number of rows in the title and header, and the second
#               element giving the number of rows in the table body.
# ncol ........ integer vector of length two, with the first element giving
#               the number of columns containing auxiliary information, and
#               the second element giving the number of columns containing
#               actual results.
# version ..... character string specifying whether the table should have the
#               appearance of recent SPSS version ("modern") or older ones
#               ("legacy").

# Modern theme:
# Columns with auxiliary information on the results will receive text in color
# 'blueSPSS' and background in color 'graySPSS'.  By default, there will not be
# a border before the first column or between these columns, nor with the first
# remaining column.  The remaining columns will receive background in color in
# color 'lightgraySPSS'.  By default, there will be lines in between those
# columns in color 'darkgraySPSS', but not after the last column.

# Legacy theme:
# Columns with auxiliary information on the results will receive a border
# before the first column, but not in between these columns.  There will be
# border with the first remaining column, as well as between the remaining
# columns and after the last column.


latexBeginTabular <- function(alignment, border, nrow, ncol,
                              version = "modern") {
  legacy <- version == "legacy"
  if (legacy) {
    # specify environment
    environment <- "tabular"
    # specify borders
    border <- ifelse(border, "|", "")
    # full specifications per column except left border of first column
    specs <- paste0(alignment, border[-1])
  } else {
    # specify environment
    environment <- "NiceTabular"
    # specify borders
    border <- ifelse(border, "/", "")
    # define font colors for the info columns
    font <- rep.int(c(">{\\color{blueSPSS}}", ""), ncol)
    # full specifications per column except left border of first column
    specs <- paste0(font, alignment, border[-1])
  }
  # create LaTeX statements
  lines <- paste0("\\begin{", environment, "}{", border[1],
         paste0(specs, collapse = ""), "}\n")
  if (!legacy) {
    lines <- c("\\NiceMatrixOptions{custom-line = {letter = /, color = darkgraySPSS}}\n",
               lines, "\\CodeBefore\n",
               sprintf("  \\rectanglecolor{graySPSS}{%d-1}{%d-%d}\n",
                       nrow[1]+1, nrow[1]+nrow[2], ncol[1]),
               sprintf("  \\rectanglecolor{lightgraySPSS}{%d-%d}{%d-%d}\n",
                       nrow[1]+1, ncol[1]+1, nrow[1]+nrow[2], ncol[1]+ncol[2]),
               "\\Body\n")
  }
  lines
}

latexEndTabular <- function(version = "modern") {
  environment <- if (version == "legacy") "tabular" else "NiceTabular"
  sprintf("\\end{%s}\n", environment)
}


## function to create LaTeX \multicolumn statement for the main title
# (multiple statements if any line breaks are specified)
# to alignment and borders
# text ........ character string giving the text to be written inside the
#               merged cell.
# columns ..... number of subsequent columns to merge.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "c" for centered.

latexTitle <- function(text, columns = 1, alignment = "c", version = "modern") {
  # split text string according to line breaks
  text <- strsplit(text, split = "\n", fixed = TRUE)[[1]]
  # # create LaTeX statement
  # if (version == "legacy") {
    # create \multicolumn statement
    sprintf("\\multicolumn{%d}{%s}{\\textbf{%s}} \\\\\n",
          columns, alignment, text)
  # } else {
  #   # create \Block statement
  #   command <- sprintf("\\Block[%s]{1-%d}{\\textbf{%s}}",
  #                      alignment, columns, text)
  #   # \Block doesn't work like \multicolumn: for proper alignment of the table,
  #   # we still need to add column separators '&' for all merged cells
  #   if (columns > 1) {
  #     suffix <- paste(rep.int("&", columns - 1), collapse = " ")
  #     command <- paste(command, suffix)
  #   }
  #   # add line end
  #   command <- paste(command,  "\\\\\n")
  #   # return LaTeX statement
  #   command
  # }
}


## function to create LaTeX \multicolumn statements
# (multiple statements if any line breaks are specified)
# to alignment and borders
# text ........ character string giving the text to be written inside the
#               merged cells.
# columns ..... number of subsequent columns to merge.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "l" for left-aligned.

latexMulticolumn <- function(text, columns = 1, alignment = "l",
                             version = "modern") {
  # split text string according to line breaks
  text <- strsplit(text, split = "\n", fixed = TRUE)[[1]]
  # # create LaTeX statement
  # if (version == "legacy") {
    # create \multicolumn statement
    sprintf("\\multicolumn{%d}{%s}{%s} \\\\\n", columns, alignment, text)
  # } else {
  #   # create \Block statement
  #   command <- sprintf("\\Block[%s]{1-%d}{%s}", alignment, columns, text)
  #   # \Block doesn't work like \multicolumn: for proper alignment of the table,
  #   # we still need to add column separators '&' for all merged cells
  #   if (columns > 1) {
  #     suffix <- paste(rep.int("&", columns - 1), collapse = " ")
  #     command <- paste(command, suffix)
  #   }
  #   # add line end
  #   command <- paste(command,  "\\\\\n")
  #   # return LaTeX statement
  #   command
  # }
}


## function to create a header cell, which in many cases contains a
## \multicolumn statement that also defines the appearance with respect
## to alignment and borders
# text ........ character string giving the text to be written inside the
#               merged cell.
# columns ..... number of subsequent columns to merge.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "c" for centered.
# left ........ logical vector indicating whether to draw a left border.
# right ....... logical vector indicating whether to draw a right border.
# version ..... character string specifying whether the cell should have the
#               appearance of recent SPSS version ("modern") or older ones
#               ("legacy").

latexHeaderCell <- function(text = "", columns = 1, alignment = "c",
                            left = FALSE, right = FALSE, version = "modern") {
  # nothing to do for a single cell with an empty string
  if (text == "" && columns == 1) return("")
  # create LaTeX statement
  if (version == "legacy") {
    # if requested, define left and right borders
    left <- if (left) "|" else ""
    right <- if (right) "|" else ""
    # full specification of the merged cell
    spec <- paste0(left, alignment, right)
    # create \multicolumn statement
    sprintf("\\multicolumn{%d}{%s}{%s}", columns, spec, text)
  } else {
    # # create \Block statement and use text color 'blueSPSS'
    # command <- sprintf("\\Block[%s]{1-%d}{\\textcolor{blueSPSS}{%s}}",
    #                    alignment, columns, text)
    # # \Block doesn't work like \multicolumn: for proper alignment of the table,
    # # we still need to add column separators '&' for all merged cells
    # if (columns > 1) {
    #   suffix <- paste(rep.int("&", columns - 1), collapse = " ")
    #   command <- paste(command, suffix)
    # }
    # # return LaTeX statement
    # command
    # create \multicolumn statement and use text color 'blueSPSS'
    sprintf("\\multicolumn{%d}{%s}{\\textcolor{blueSPSS}{%s}}",
            columns, alignment, text)
  }
}


## function to create a info cell, which may contain a \makecell statement
## to break up the cell according to defined line breaks
# text ........ character string giving the text to be written inside the
#               cell.  If it contains any line breaks, a \makecell statement
#               will be created.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "l" for left-aligned.

latexInfoCell <- function(text = "", alignment = "l", version = "modern") {
  if (grepl("\n", text, fixed = TRUE)) {
    # currently, vertical alignment is always set to top-alignment
    latexText <- gsub("\n", "\\\\", text, fixed = TRUE)
    if (version == "legacy") {
      sprintf("\\makecell[t%s]{%s}", alignment, latexText)
    } else sprintf("\\Block[%s,t]{}{%s}", alignment, latexText)
  } else text
}


## function to combine footnote markers that are in the same position and to
## add LaTeX formatting
parseFootnoteMarkers <- function(row, column, marker) {
  # remove empty markers
  keep <- which(marker != "")
  if (length(keep) == 0) return(list())
  row <- row[keep]
  column <- column[keep]
  marker <- marker[keep]
  # prefix and suffix of LaTeX statement
  prefix <- "$^{\\text{"
  suffix <- "}}$"
  # format markers for main title
  keep <- which(row == "main")
  if (length(keep) == 0) main <- NULL
  else main <- paste0(prefix, paste(marker[keep], collapse = ","), suffix)
  # format markers for sub title
  keep <- which(row == "sub")
  if (length(keep) == 0) sub <- NULL
  else sub <- paste0(prefix, paste(marker[keep], collapse = ","), suffix)
  # format markers for table body
  keep <- which(!(row %in% c("main", "sub")))
  if (length(keep) == 0) body <- NULL
  else {
    # keep only markers for table body
    row <- as.integer(row[keep])
    column <- as.integer(column[keep])
    marker <- marker[keep]
    # find unique index pairs
    body <- unique(data.frame(row, column))
    markers <- mapply(function(i, j) {
      paste(marker[row == i & column == j], collapse = ",")
    }, i = body$row, j = body$column, USE.NAMES = FALSE)
    body$marker <- paste0(prefix, markers, suffix)
  }
  # return list of markers to be inserted in each section
  list(main = main, sub = sub, body = body)

}

## function to format text of footnotes
parseFootnoteText <- function(text = "", marker = "") {
  # we only need to do something if the marker is a non-empty string
  if (marker != "") {
    # prefix to add before footnote text
    prefix <- paste0(marker, ". ")
    # if there are line breaks, the subsequent lines should be indented
    # accordingly, which can be done with \phantom{}
    phantom <- paste0("\\phantom{", prefix, "}")
    text <- gsub("\n", paste0("\n", phantom), text, fixed = TRUE)
    # add prefix before footnote
    text <- paste0(prefix, text)
  }
  # return parsed footnote
  text
}


## functions to define various line types

latexLine <- function() "\\hline\n"

latexPartialLine <- function(first, last) {
  sprintf("\\cline{%d-%d}\n", first, last)
}


latexTopLine <- latexBottomLine <- function(version = "modern") {
  legacy <- version == "legacy"
  line <- latexLine()
  if (!legacy) line <- paste0("\\arrayrulecolor{black}", line)
  line
}

latexMajorLine <- function(version = "modern") {
  legacy <- version == "legacy"
  line <- latexLine()
  if (!legacy) line <- paste0("\\arrayrulecolor{darkgraySPSS}", line)
  line
}

latexMinorLine <- function(first = NULL, last = NULL, version = "modern") {
  legacy <- version == "legacy"
  if (is.null(first) || is.null(last)) line <- latexLine()
  else line <- latexPartialLine(first, last)
  if (!legacy) line <- paste0("\\arrayrulecolor{darkgraySPSS}", line)
  line
}
