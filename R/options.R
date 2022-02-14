

# function to initialize and set default values for options
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


## object for package options
#' @export
r2spssOptions <- initializeOptions(version = "modern")
