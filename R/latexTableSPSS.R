# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


## header ... can also be character vector with different names or a list that
##            defines the hierarchy, e.g.:
##            list("Mean", "Std. Error",
##                 "Confidence Interval" = c("Lower\nBound", "Upper\nBound"))
##            Line separators break up the header into different lines

latexTableSPSS <- function(object, ...) UseMethod("latexTableSPSS")

latexTableSPSS.data.frame <- function(object, main = NULL, sub = NULL,
                                      header = TRUE, label = NULL,
                                      rowNames = TRUE, info = NULL,
                                      alignment = NULL, width = NULL,
                                      border = NULL, footnotes = NULL,
                                      major = NULL, minor = NULL,
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
  ##
  ## Even better may be to add arguments for where to print major and minor
  ## grid lines.  For the first it can be a simple integer vector giving the
  ## line number.  For the latter it can also be a matrix with line number, as
  ## well as the indices of the first and last column.  This is a much better
  ## solution, as it would allow to add an argument to other functions that
  ## determines whether to draw the minor grid lines (which SPSS does, but
  ## which can be distract away from the information in exams or assignments).

  ## FIXME: major and minor lines should be in color 'darkgraySPSS' for modern
  ##        theme, but I haven't figured out yet how to specify this.  The
  ##        lines above and below the table body should remain the default
  ##        color (typically black).

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
    if (is.list(header)) {
      if (length(unlist(header, use.names = FALSE)) != columns) {
        stop(sprintf("'header' must have in total %d elements", columns))
      }
    } else if (is.character(header)) {
      if (length(header) != columns) {
        stop(sprintf("'header' must have length %d", columns))
      }
    } else stop("'header' must be TRUE/FALSE, a list, or a character vector")
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
  # check column width specifications
  if (is.null(width)) width <- rep.int("", columns)
  else {
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

  ## write \begin{tabular} statement
  cat(latexBeginTabular(columns, info, alignment = alignment$table,
                        width = width, border = border, theme = theme))

  ## if supplied, write main and sub title
  if (writeMain) {
    # if supplied, insert footnote markers
    if (writeFootnotes && !is.null(insertFootnotes$main)) {
      main <- paste0(main, insertFootnotes$main)
    }
    # write main title
    cat("\\noalign{\\smallskip}\n")
    cat(latexTitle(main, columns = columns, alignment = "c"), sep = "")
    cat("\\noalign{\\smallskip}\n")
  }
  if (writeSub) {
    # if supplied, insert footnote markers
    if (writeFootnotes && !is.null(insertFootnotes$sub)) {
      sub <- paste0(sub, insertFootnotes$sub)
    }
    # write main title
    cat(latexMulticolumn(sub, columns = columns, alignment = "l"), sep = "")
  }

  ## if supplied, write table header
  if (writeHeader) {
    # for legacy theme, draw line above header
    if (legacy) cat("\\hline\n")
    # parse header layout
    leftBorder <- c(border[1], rep.int(FALSE, columns-1))
    rightBorder <- border[-1]
    headerList <- parseHeaderLayout(header, alignment = alignment$header,
                                    left = leftBorder,
                                    right = rightBorder)
    # loop over levels in header layout
    for (level in headerList) {
      # obtain matrix of header cells based on line breaks
      # note: strsplit() transforms "" to character()
      headerText <- strsplit(level$header, "\n", fixed = TRUE)
      headerRows <- vapply(headerText, length, numeric(1))
      nHeaderRows <- max(headerRows)
      if (nHeaderRows <= 1) headerText <- matrix(level$header, nrow = 1)
      else {
        headerText <- mapply(function(text, rows) {
          c(rep.int("", nHeaderRows - rows), text)
        }, text = headerText, rows = headerRows, USE.NAMES = FALSE)
      }
      # write rows of header cells
      for (i in seq_len(nHeaderRows)) {
        headerCells <- mapply(latexHeaderCell, text = headerText[i, ],
                              columns = level$columns,
                              alignment = level$alignment,
                              left = level$left, right = level$right,
                              MoreArgs = list(theme = theme),
                              USE.NAMES = FALSE)
        cat(paste(headerCells, collapse = " & "), "\\\\\n")
      }
      # for legacy theme, add partial lines under merged cells
      merged <- level$merged
      if (legacy && !is.null(merged)) {
        for (j in seq_len(nrow(merged))) {
            cat("\\cline{", merged[j, "first"], "-", merged[j, "last"], "}\n",
                sep = "")
        }
      }
    }
    # draw line to separate header from table body
    cat("\\hline\n")
  }

  ## format and write table body
  # format everything nicely
  # if (legacy) {
  #   args <- list(object, ...)
  #   # args$x <- object
  #   if (is.null(args$pValue)) args$pValue <- FALSE
  #   formatted <- do.call(formatSPSS, args)
  # } else formatted <- formatSPSS(object, ...)
  if (legacy) formatted <- formatSPSS(object, ...)
  else {
    # for modern theme, set a reasonable default to format p-values nicely
    args <- list(object, ...)
    if (is.null(args$pValue)) {
      args$pValue <- grepl("Sig.", names(object), fixed = TRUE)
    }
    formatted <- do.call(formatSPSS, args)
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
  for (i in seq.int(nrow(formatted))) {
    # write current row
    if (i == 1) {
      cat(if (addLabel) paste(label, "&"),
          if (addRowNames) paste(rowNames[i], "&"),
          paste0(formatted[i, ], collapse = " & "),
          "\\\\\n")
    } else {
      cat(if (addLabel) " &",
          if (addRowNames) paste(rowNames[i], "&"),
          paste0(formatted[i, ], collapse = " & "),
          "\\\\\n")
    }
    # if requested, draw major grid line
    if (drawMajor && (i %in% major)) cat("\\hline\n")
    # if requested, draw minor grid line
    if (drawMinor) {
      if (partialMinor) {
        which <- match(i, minor$row)
        if (!is.na(which)) {
          cat("\\cline{", minor[which, "first"], "-", minor[which, "last"],
              "}\n", sep = "")
        }
      } else if (i %in% minor) cat("\\hline\n")
    }
  }
  # draw line below table body
  cat("\\hline\n")

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
  cat("\\end{tabular}\n")

}
