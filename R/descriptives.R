#' @export
descriptives <- function(x) {
  # initializations
  x <- as.data.frame(x)
  classes <- vapply(x, function(x) class(x)[1], character(1))
  n <- sum(complete.cases(x))
  # compute minimum, maximum, mean and standard deviation for each variable
  desc <- do.call(rbind, lapply(x, .descriptives))
  row.names(desc) <- names(x)
  # return descriptives
  out <- list(classes=classes, descriptives=desc, n=n)
  class(out) <- "descriptives"
  out
}

# compute minimum, maximum, mean and standard deviation for a variable
.descriptives <- function(x) {
  # initializations
  ok <- is.finite(x)
  x <- x[ok]
  # compute descriptives
  n <- length(x)
  range <- range(x)
  mean <- mean(x)
  sd <- sd(x)
  # return data frame
  data.frame(N=n, Minimum=range[1], Maximum=range[2], Mean=mean,
             "Std. Deviation"=sd, check.names=FALSE)
}

#' @export
print.descriptives <- function(x, digits = 2, ...) {
  # format descriptives
  d <- ifelse(x$classes == "integer", 0, digits)
  formatted <- cbind(
    formatSPSS(x$descriptives[, "N"]),
    formatSPSS(x$descriptives[, c("Minimum", "Maximum")], digits=d),
    formatSPSS(x$descriptives[, c("Mean", "Std. Deviation")], digits=digits)
  )
  # initialize LaTeX table
  cat("\\begin{tabular}{|l|r|r|r|r|r|}\n")
  # print table header
  cat("\\multicolumn{6}{c}{\\textbf{Descriptive Statistics}} \\\\\n")
  cat("\\noalign{\\smallskip}\\hline\n")
  cat(" & & & & & \\multicolumn{1}{|c|}{Std.} \\\\\n")
  cat(" & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Minimum} & \\multicolumn{1}{|c|}{Maximum} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} \\\\\n")
  cat("\\hline\n")
  # print descriptives for each variable
  for (variable in rownames(formatted)) {
    cat(variable, "&", paste(formatted[variable, ], collapse=" & "), "\\\\\n")
  }
  # print complete cases
  cat("Valid N (listwise) &", x$n, "& & & & \\\\\n")
  # finalize LaTeX table
  cat("\\hline\n")
  cat("\\end{tabular}\n")
}