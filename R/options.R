# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

# internal function to initialize and set default values for options
initializeOptions = function(...) {

  # assign default values to options (they live in this environment)
  values <- list(...)

  # accessor function to retrieve current options (or a selection)
  get <- function(which, drop = TRUE) {
    if (missing(which)) values
    else if (length(which) == 1 && drop) values[[which]]
    else values[which]
  }

  # accessor function to get (selected) current options
  set = function(...) {
    # combine supplied options into list and extract names
    newValues <- list(...)
    optionNames <- names(newValues)
    # check if a list is supplied as a single unnamed argument
    if (length(newValues) == 1 && is.null(optionNames) && is.list(newValues[[1]])) {
      newValues <- newValues[[1]]
      optionNames <- names(newValues)
    }
    # check if there is anything to do
    if (length(newValues) == 0 || is.null(optionNames)) {
      warning("no names or values of options supplied; no options were set",
              call. = FALSE)
    } else {
      # check if supplied options are meaningful
      keep <- optionNames %in% names(values)
      newValues <- newValues[keep]
      optionNames <- optionNames[keep]
      if (length(newValues) == 0) {
        warning("supplied options do not exist; no options were set",
                call. = FALSE)
      } else if (!all(keep)) {
        warning("some supplied options do not exist; those were not set",
                call. = FALSE)
      } else values[optionNames] <<- newValues
    }
    # don't return anything
    invisible()
  }

  # return list of accessor functions
  list(get = get, set = set)
}

#' Current options for r2spss
#'
#' Retrieve or set global options for package \pkg{r2spss} (within the current
#' \R session) via accessor functions.
#'
#' Currently, the only available option is \code{version}, which controls
#' tables and plots should mimic the content and look of recent SPSS versions
#' (\code{"modern"}) or older versions (<24; \code{"legacy"}).
#'
#' @format A list with the following two components:
#' \describe{
#'   \item{\code{get(which, drop = TRUE)}}{an accessor function to retrieve
#'   current options, which are usually returned as a named list.  Argument
#'   \code{which} allows to select which options to retrieve.  If a single
#'   option is selected, argument \code{drop} indicates whether only its value
#'   should be returned (\code{TRUE}) or a list of length one (\code{FALSE}).}
#'   \item{\code{set(...)}}{an accessor function to set certain options using
#'   \code{name = value} pairs.}
#' }
#'
#' @author Andreas Alfons
#'
#' @examples
#' # retrieve list of options:
#' r2spssOptions$get()
#'
#' # retrieve a single option:
#' r2spssOptions$get("version")
#'
#' \dontrun{
#'
#' # set an option:
#' r2spssOptions$set(version = "legacy")
#' }
#'
#' @keywords utilities
#'
#' @export

r2spssOptions <- initializeOptions(version = "modern")
