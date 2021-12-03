# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


## If header is given as a list, parse the structure and return a list with the
## necessary information for each level to write the header cells of the latex
## table.  Otherwise the list contains only one component, which contains all
## the information for the only level of header cells.

parseHeaderLayout <- function(object, ...) UseMethod("parseHeaderLayout")

parseHeaderLayout.list <- function(object, alignment, left, right, ...) {
  ## initializations
  if (length(object) > 3) stop("header can have at most three levels")
  target <- c("first", "last", "text")
  labels <- NULL

  ## for three header levels, construct top level and treat the remaining
  ## levels as usual
  if (length(object) == 3) {

    # some checks
    labels <- paste(c("first", "second", "third"), "header element")
    checkDataFrame(object[[1]], label = labels[1], targetNames = target)
    # parse top level header
    # top <- object[[1]]
    # top <- parseHeaderLayout(top$text, alignment = alignment,
    #                          left = left, right = right,
    #                          columns = top$last - top$first + 1)
    top <- parseHeaderLayout(object[[1]], alignment = alignment,
                             left = left, right = right)
    # remove top level header from object
    object <- object[-1]
    labels <- labels[-1]

  } else top <- NULL

  ## workhorse part for converting two header levels
  if (length(object) == 2) {

    # some checks
    if (is.null(labels)) labels <- paste(c("first", "third"), "header element")
    checkDataFrame(object[[1]], label = labels[1], targetNames = target)
    checkCharacter(object[[2]], label = labels[2])

    # extract first (top) and second (bottom) layer
    first <- object[[1]]
    second <- object[[2]]
    n <- c(nrow(first), length(second))

    # split strings according to line breaks
    firstList <- strsplit(first$text, "\n", fixed = TRUE)
    firstList <- lapply(firstList, function(t) if (length(t) == 0) "" else t)
    secondList <- strsplit(second, "\n", fixed = TRUE)
    secondList <- lapply(secondList, function(t) if (length(t) == 0) "" else t)

    # determine heights of first and second header level
    firstHeight <- vapply(firstList, length, integer(1))
    secondHeight <- vapply(secondList, length, integer(1))

    # compute overall heights and maximum height
    columns <- first$last - first$first + 1
    height <- rep.int(firstHeight, times = columns) + secondHeight
    maxHeight <- max(height)

    # loop over first level columns and fill up children in second level and
    # the first level cell itself such that all columns have maximum height
    header <- lapply(seq_len(n[1]), function(j) {
      # find indices of children in second level
      which <- seq(from = first[j, "first"], to = first[j, "last"])
      # obtain text and height in each level
      parentText <- firstList[[j]]
      parentHeight <- firstHeight[j]
      childrenText <- secondList[which]
      childrenHeight <- secondHeight[which]
      # make children in second level the same height by adding empty strings
      targetHeight <- max(childrenHeight)
      childrenList <- mapply(function(t, h) {
        if (h < targetHeight) c(rep.int("", targetHeight - h), t)
        else t
      }, t = childrenText, h = childrenHeight, SIMPLIFY = FALSE)
      childrenMat <- do.call(cbind, childrenList)
      # make overall height the same by adding empty strings to parent cell
      targetHeight <- c(maxHeight - targetHeight, targetHeight)
      if (parentHeight < targetHeight[1]) {
        parentText <- c(rep.int("", targetHeight[1] - parentHeight), parentText)
      }
      # return list with necessary information
      column <- list(first = parentText, second = childrenMat,
                     height = targetHeight, which = which)
      if ((first[j, "last"] > first[j, "first"]) || parentText != "") {
        column$merged <- first[j, c("first", "last")]
      }
      column
    })
    # obtain number of columns, alignment, and borders for each level
    columns <- list(first = columns, second = rep.int(1, n[2]))
    alignment <- list(first = getMergedAlignment(columns$first, alignment),
                      second = alignment)
    left <- list(first = left[first$first], second = left)
    right <- list(first = right[first$last], second = right)

    ## construct a list of the relevant information per row
    header <- lapply(seq_len(maxHeight), function(i) {
      # loop over the first level columns
      text <- col <- align <- l <- r <- m <- NULL
      for (j in seq_len(n[1])) {
        current <- header[[j]]
        if (i <= current$height[1]) {
          # add first level information
          text <- c(text, current$first[i])
          col <- c(col, columns$first[j])
          align <- c(align, alignment$first[j])
          l <- c(l, left$first[j])
          r <- c(r, right$first[j])
          if (i == current$height[1]) m <- rbind(m, current$merged)
        } else {
          # add second level information
          which <- current$which
          text <- c(text, current$second[i - current$height[1], ])
          col <- c(col, columns$second[which])
          align <- c(align, alignment$second[which])
          l <- c(l, left$second[which])
          r <- c(r, right$second[which])
        }
      }
      # return list of relevant information
      list(text = text, columns = col, alignment = align,
           left = l, right = r, merged = m)
    })

  } else stop("'object' should have two elements")

  ## return parsed header
  if (is.null(top)) header
  else c(top, header)
}

parseHeaderLayout.data.frame <- function(object, alignment, left, right, ...) {
  # initializations
  checkDataFrame(object, label = "'object",
                 targetNames = c("first", "last", "text"))
  # obtain number of columns for each element
  columns <- object$last - object$first + 1
  # obtain alignment of each element
  align <- getMergedAlignment(columns, alignment)
  # call default method
  header <- parseHeaderLayout(object$text, alignment = align,
                              left = left[object$first],
                              right = right[object$last],
                              columns = columns)
  # determine which cells are merged (lines to draw for legacy theme)
  keep <- (object$last > object$first) | object$text != ""
  # add info on merged cells to last line
  last <- length(header)
  header[[last]]$merged <- object[keep, c("first", "last")]
  # return parsed header
  header
}

parseHeaderLayout.default <- function(object, alignment, left, right,
                                      columns = NULL, ...) {
  # initializations
  checkCharacter(object, label = "'object'")
  if (is.null(columns)) columns <- rep.int(1, length(object))
  # split according to line breaks and determine maximum height
  textList <- strsplit(object, "\n", fixed = TRUE)
  textHeight <- vapply(textList, length, numeric(1))
  maxHeight <- max(textHeight)
  # convert to matrix containing the full rows
  if (maxHeight <= 1) textMat <- matrix(object, nrow = 1)
  else {
    textMat <- mapply(function(text, height) {
      c(rep.int("", maxHeight - height), text)
      }, text = textList, height = textHeight, USE.NAMES = FALSE)
  }
  # split into list and add all required information
  lapply(seq_len(maxHeight), function(i) {
    list(text = textMat[i, ], columns = columns, alignment = alignment,
         left = left, right = right)
  })
}


# utility function to check data frames
checkDataFrame <- function(x, label = NULL, targetNames = NULL) {
  # initializations
  if (is.null(label)) label <- "'x'"
  # check class and number of rows
  if (!(is.data.frame(x) && nrow(x) > 0)) {
    stop(sprintf("%s should be a data frame with nonzero number of rows", label),
         call. = FALSE)
  }
  # if supplied, check target names
  if (!is.null(targetNames)) {
    ok <- vapply(targetNames, "%in%", logical(1), names(x),
                 USE.NAMES = FALSE)
    if (!all(ok)) {
      stop(sprintf("%s should have columns ", label),
           paste(paste0("'", targetNames, "'"), collapse = ", "),
           call. = FALSE)
    }
  }
}

# utility function to check character vectors
checkCharacter <- function(x, label = NULL) {
  # initializations
  if (is.null(label)) label <- "'x'"
  # check class and number of rows
  if (!(is.character(x) && length(x) > 0)) {
    stop(sprintf("%s should be a character vector of nonzero length", label),
         call. = FALSE)
  }
}

# utility function to obtain alignment of merged cells based on
# alignment of individual cells
getMergedAlignment <- function(columns, alignment) {
  # split alignment specifiers according top level cell membership
  group <- rep.int(seq_along(columns), times = columns)
  alignmentList <- split(alignment, group)
  # obtain alignment of merged cells
  vapply(alignmentList, function(align) {
    if (length(align) == 1) align
    else {
      unique <- unique(align)
      if (length(unique) == 1) unique
      else "c"
    }
  }, character(1), USE.NAMES = FALSE)
}
