#' @export
chisqTest <- function(data, variables, type = NULL) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  if (length(variables) < 2) stop("two variables to test must be specified")
  # check factors
  row <- as.factor(data[, variables[1]])
  col <- as.factor(data[, variables[2]])
  r <- nlevels(row)
  c <- nlevels(col)
  if (r < 2 || c < 2) {
    stop("chi-square test requires at least two groups in each factor")
  }
  ok <- !is.na(row) & !is.na(col)
  row <- row[ok]
  col <- col[ok]
  # compute observed and expected frequencies
  observed <- table(row, col, dnn=variables[1:2])
  n <- sum(observed)
  if (n == 0) stop("at least one cell must be nonzero")
  expected <- outer(rowSums(observed), colSums(observed), "*") / n
  df <- (r-1) * (c-1)
  # perform chi-square test
  stat <- sum((observed - expected)^2 / expected)
  p <- pchisq(stat, df=df, lower.tail=FALSE)
  chisq <- list(statistic=stat, parameter=df, p.value=p)
  # perform likelihood ratio test
  keep <- observed != 0
  stat <- 2 * sum(observed[keep] * log(observed[keep]/expected[keep]))
  p <- pchisq(stat, df=df, lower.tail=FALSE)
  lr <- list(statistic=stat, parameter=df, p.value=p)
  # return results
  out <- list(chisq=chisq, lr=lr, observed=observed, expected=expected, n=n,
              r=r, c=c, variables=variables[1:2], type="independence")
  class(out) <- "chisqTest"
  out
}

#' @export
print.chisqTest <- function(x, digits = c(1, 3),
                            statistics = c("frequencies", "test"),
                            ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)

  ## print LaTeX table for frequencies
  if ("frequencies" %in% statistics) {
    # extract frequencies
    observed <- x$observed
    expected <- x$expected
    # add totals
    observed <- cbind(observed, Total=rowSums(observed))
    observed <- rbind(observed, Total=colSums(observed))
    expected <- cbind(expected, Total=rowSums(expected))
    expected <- rbind(expected, Total=colSums(expected))
    # format frequencies
    storage.mode(observed) <- "integer"
    observed <- formatSPSS(observed)
    expected <- formatSPSS(expected, digits=digits[1])
    # initialize LaTeX table
    cat("\n")
    cat("\\begin{tabular}{|lll|", paste0(rep.int("r", x$c), collapse="|"), "|r|}\n", sep="")
    # print table header
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{", x$c+4, "}{c}{\\textbf{", x$variable[1], " * ", x$variable[2], " Crosstabulation}} \\\\\n", sep="")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & & & \\multicolumn{", x$c, "}{|c|}{", x$variable[2], "} & \\\\\n", sep="")
    cat("\\cline{4-", x$c+3, "}\n", sep="")
    cat(" & & &", paste0(paste0("\\multicolumn{1}{|c|}{", colnames(observed), "}"), collapse=" & "), "\\\\\n")
    cat("\\hline\n")
    cat(x$variables[1])
    for (i in seq_len(x$r)) {
      cat(" &", rownames(observed)[i], "& Count &", paste0(observed[i,], collapse=" & "), "\\\\\n")
      cat(" & & Expected Count &", paste0(expected[i,], collapse=" & "), "\\\\\n")
      if (i < x$r) cat("\\cline{2-", x$c+4, "}\n", sep="")
    }
    cat("\\hline\n")
    cat(rownames(observed)[x$r+1], "& & Count &", paste0(observed[x$r+1,], collapse=" & "), "\\\\\n")
    cat(" & & Expected Count &", paste0(expected[x$r+1,], collapse=" & "), "\\\\\n")
    # finalize LaTeX table
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for chi-square test
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    rn <- c("Pearson Chi-Square", "Likelihood Ratio")
    test <- data.frame(Value=c(x$chisq$statistic, x$lr$statistic),
                       df=as.integer(c(x$chisq$parameter, x$lr$parameter)),
                       "Asymp. Sig. (2-sided)"=c(x$chisq$p.value, x$lr$p.value),
                       check.names=FALSE, row.names=rn)
    formatted <- formatSPSS(test, digits=digits[2])
    formatted["Pearson Chi-Square", "Value"] <-
      paste0(formatted["Pearson Chi-Square", "Value"], "$^{\\text{a}}$")
    # initialize LaTeX table
    cat("\\begin{tabular}{|l|r|r|r|}\n")
    # print table header
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{4}{c}{\\textbf{Chi-Square Tests}} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & & & \\multicolumn{1}{|c|}{Asymp. Sig.} \\\\\n")
    cat(" & \\multicolumn{1}{|c|}{Value} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{(2-sided)} \\\\\n")
    cat("\\hline\n")
    # print table
    for (rn in rownames(formatted)) {
      cat(rn, "&", paste0(formatted[rn,], collapse=" & "), "\\\\\n")
    }
    cat("N of Valid Cases &", x$n, "& & \\\\\n")
    cat("\\hline\n")
    # check too small expected counts
    nTooSmall <- sum(x$expected < 5)
    pTooSmall <- nTooSmall / length(x$expected)
    smallest <- min(x$expected)
    cat("\\multicolumn{4}{l}{a. ", nTooSmall, " cells (", format(pTooSmall, digits=digits[1]), "\\%) have expected counts less than 5.} \\\\\n", sep="")
    cat("\\multicolumn{4}{l}{\\phantom{a. }The minimum expected count is ", formatSPSS(smallest, digits=digits[2]), ".} \\\\\n", sep="")
    # finalize LaTeX table
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
  }
}
