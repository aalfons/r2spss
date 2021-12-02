# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------


## function to add line breaks in character strings to make sure that a given
## character limit is not exceeded per line
# text .... a character vector for which each element will be wrapped
# limit ... integer vector giving the character limit for each element of 'text'

wrapText <- function(text, limit = 66) {
  # initializations
  n <- length(text)
  limit <- rep_len(limit, n)
  # split text according to white space
  partsList <- strsplit(text, "\\s+", fixed = FALSE)
  # loop over list
  mapply(function(parts, limit) {
    # add parts to a line as long as there is space (given by 'limit'),
    # and start a new line when running out of space
    lines <- character(0)
    while(length(parts) > 0) {
      if (length(parts) == 1) {
        # only one part left, so add it as a new line and we're done
        add <- 1
      } else {
        # temporarily add space before every part except the first one
        strings <- c(parts[1], paste(" ", parts[-1], sep = ""))
        # determine how many predictors have space
        width <- nchar(strings)
        add <- which(cumsum(width) <= limit)
        # if the first part is too long it needs to be added anyway
        if (length(add) == 0) add <- 1
      }
      # add a new line
      lines <- c(lines, paste(parts[add], collapse = " "))
      # remove the parts that have just been written to the new line
      parts <- parts[-add]
    }
    # add line break in between lines
    paste(lines, collapse = "\n")
  }, parts = partsList, limit = limit)
}
