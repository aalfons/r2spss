# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


## header ... can also be character vector with different names or a list
##            that defines the hierarchy  (see, e.g. tTest() for an example
##            on how to do this).  Line separators break up the header into
##            different lines.

## @export
# toLatex <- function(object, ...) UseMethod("toLatex")

#' @importFrom utils toLatex

toLatex.data.frame <- function(object, main = NULL, sub = NULL, header = TRUE,
                               label = NULL, rowNames = TRUE, info = NULL,
                               alignment = NULL, border = NULL,
                               footnotes = NULL, major = NULL, minor = NULL,
                               theme = c("modern", "legacy"), ...) {

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
    if (!is.numeric(major) || length(major) == 0) {
      stop("'footnotes' must be a character vector of nonzero length")
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
        stop("'footnotes' must be an integer vector or data.frame")
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
    latexMain <- latexTitle(main, columns = columns, alignment = "c")
  } else latexMain <- NULL
  if (writeSub) {
    # if supplied, insert footnote markers
    if (writeFootnotes && !is.null(insertFootnotes$sub)) {
      sub <- paste0(sub, insertFootnotes$sub)
    }
    # get LaTeX statement(s)
    latexSub <- latexMulticolumn(sub, columns = columns, alignment = "l")
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
                        ncol = c(info, results), theme = theme),
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
                            MoreArgs = list(theme = theme),
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
    cat(latexTopLine(theme = theme))
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
      cat(if (addLabel) paste(latexInfoCell(label, alignLabel, theme = theme), "&"),
          if (addRowNames) paste(latexInfoCell(rowNames[i], alignName, theme = theme), "&"),
          paste0(formatted[i, ], collapse = " & "),
          "\\\\\n")
    } else {
      cat(if (addLabel) " &",
          if (addRowNames) paste(latexInfoCell(rowNames[i], alignName, theme = theme), "&"),
          paste0(formatted[i, ], collapse = " & "),
          "\\\\\n")
    }
    # if requested, draw major grid line
    if (drawMajor && (i %in% major)) cat(latexMajorLine(theme = theme))
    # if requested, draw minor grid line
    if (drawMinor) {
      if (partialMinor) {
        which <- match(i, minor$row)
        if (!is.na(which)) {
          cat(latexMinorLine(minor[which, "first"], minor[which, "last"],
                             theme = theme))
        }
      } else if (i %in% minor) cat(latexMinorLine(theme = theme))
    }
  }
  # draw line below table body
  cat(latexBottomLine(theme = theme))

  ## if supplied, write footnotes
  if (writeFootnotes) {
    if (is.data.frame(footnotes)) {
      # writing footnotes is more elaborate if markers are supplied
      for (i in seq_len(nrow(footnotes))) {
        footnote <- parseFootnoteText(footnotes$text[i], footnotes$marker[i])
        cat(latexMulticolumn(footnote, columns = columns, alignment = "l"))
      }
    } else if (is.character(footnotes)) {
      # simply write footnotes with \mulitcolumn statements
      for (footnote in footnotes) {
        cat(latexMulticolumn(footnote, columns = columns, alignment = "l"))
      }
    }
  }

  ## write \end{tabular} statement
  cat("\\noalign{\\smallskip}\n")
  cat(latexEndTabular(theme = theme))

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
# theme ....... character string specifying whether the table should have the
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


latexBeginTabular <- function(alignment, border, nrow, ncol, theme = "modern") {
  legacy <- theme == "legacy"
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
    border <- ifelse(border, "!{\\color{darkgraySPSS}\\vrule}", "")
    # define font colors for the info columns
    font <- rep.int(c(">{\\color{blueSPSS}}", ""), ncol)
    # full specifications per column except left border of first column
    specs <- paste0(font, alignment, border[-1])
  }
  # create LaTeX statements
  lines <- paste0("\\begin{", environment, "}{", border[1],
         paste0(specs, collapse = ""), "}\n")
  if (!legacy) {
    lines <- c(lines, "\\CodeBefore\n",
               sprintf("  \\rectanglecolor{graySPSS}{%d-1}{%d-%d}\n",
                       nrow[1]+1, nrow[1]+nrow[2], ncol[1]),
               sprintf("  \\rectanglecolor{lightgraySPSS}{%d-%d}{%d-%d}\n",
                       nrow[1]+1, ncol[1]+1, nrow[1]+nrow[2], ncol[1]+ncol[2]),
               "\\Body\n")
  }
  lines
}

latexEndTabular <- function(theme = "modern") {
  environment <- if (theme == "legacy") "tabular" else "NiceTabular"
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

latexTitle <- function(text, columns = 1, alignment = "c") {
  # split text string according to line breaks
  text <- strsplit(text, split = "\n", fixed = TRUE)[[1]]
  # create LaTeX statement
  sprintf("\\multicolumn{%d}{%s}{\\textbf{%s}} \\\\\n",
          columns, alignment, text)

}


## function to create LaTeX \multicolumn statements
# (multiple statements if any line breaks are specified)
# to alignment and borders
# text ........ character string giving the text to be written inside the
#               merged cells.
# columns ..... number of subsequent columns to merge.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "l" for left-aligned.

latexMulticolumn <- function(text, columns = 1, alignment = "l") {
  # split text string according to line breaks
  text <- strsplit(text, split = "\n", fixed = TRUE)[[1]]
  # create LaTeX statement
  sprintf("\\multicolumn{%d}{%s}{%s} \\\\\n", columns, alignment, text)
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
# theme ....... character string specifying whether the cell should have the
#               appearance of recent SPSS version ("modern") or older ones
#               ("legacy").

latexHeaderCell <- function(text = "", columns = 1, alignment = "c",
                            left = FALSE, right = FALSE, theme = "modern") {
  # nothing to do for a single cell with an empty string
  if (text == "" && columns == 1) return("")
  # create LaTeX statement
  if (theme == "legacy") {
    # if requested, define left and right borders
    left <- if (left) "|" else ""
    right <- if (right) "|" else ""
    # full specification of the merged cell
    spec <- paste0(left, alignment, right)
    # create \multicolumn statement
    sprintf("\\multicolumn{%d}{%s}{%s}", columns, spec, text)
  } else {
    # create \Block statement and use text color 'blueSPSS'
    command <- sprintf("\\Block[%s]{1-%d}{\\textcolor{blueSPSS}{%s}}",
                       alignment, columns, text)
    # \Block doesn't work like \multicolumn: for proper alignment of the table,
    # we still need to add column separators '&' for all merged cells
    if (columns > 1) {
      suffix <- paste(rep.int("&", columns - 1), collapse = " ")
      command <- paste(command, suffix)
    }
    # return LaTeX statement
    command
  }
}


## function to create a info cell, which may contain a \makecell statement
## to break up the cell according to defined line breaks
# text ........ character string giving the text to be written inside the
#               cell.  If it contains any line breaks, a \makecell statement
#               will be created.
# alignment ... character string containing an alignment specifier for the
#               merged cell.  The default is "l" for left-aligned.

latexInfoCell <- function(text = "", alignment = "l", theme = "modern") {
  if (grepl("\n", text, fixed = TRUE)) {
    # currently, vertical alignment is always set to top-alignment
    latexText <- gsub("\n", "\\\\", text, fixed = TRUE)
    if (theme == "legacy") {
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


latexTopLine <- latexBottomLine <- function(theme = "modern") {
  legacy <- theme == "legacy"
  line <- latexLine()
  if (!legacy) line <- paste0("\\arrayrulecolor{black}", line)
  line
}

latexMajorLine <- function(theme = "modern") {
  legacy <- theme == "legacy"
  line <- latexLine()
  if (!legacy) line <- paste0("\\arrayrulecolor{darkgraySPSS}", line)
  line
}

latexMinorLine <- function(first = NULL, last = NULL, theme = "modern") {
  legacy <- theme == "legacy"
  if (is.null(first) || is.null(last)) line <- latexLine()
  else line <- latexPartialLine(first, last)
  if (!legacy) line <- paste0("\\arrayrulecolor{darkgraySPSS}", line)
  line
}
