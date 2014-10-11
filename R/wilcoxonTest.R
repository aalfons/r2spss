#' @export
wilcoxonTest <- function(data, variables, group = NULL) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  ## select test
  if (length(group) == 0) {
    ## signed rank test
    if (length(variables) < 2) stop("two variables to test must be specified")
    # compute differences
    d <- data[, variables[2]] - data[, variables[1]]
    ok <- is.finite(d)
    d <- d[ok]
    # compute ranks
    dnp <- d[d != 0]
    r <- rank(abs(dnp))
    # compute statistics
    negative <- dnp < 0
    positive <- dnp > 0
    count <- c(sum(negative), sum(positive))
    if (any(count == 0)) stop("all differences negative or positive")
    sum <- c(sum(r[negative]), sum(r[positive]))
    rn <- c("Negative Ranks", "Positive Ranks")
    stat <- data.frame(N=count, "Mean Rank"=sum/count, "Sum of Ranks"=sum,
                       check.names=FALSE, row.names=rn)
    # normal approximation
    n <- length(dnp)
    mu <- n*(n+1) / 4
    nTies <- table(r)
    sigma <- sqrt((n*(n+1)*(2*n+1))/24 - sum(nTies^3-nTies)/48)
    z <- (min(sum) - mu) / sigma
    p <- 2 * min(pnorm(z), pnorm(z, lower.tail=FALSE))
    test <- list(statistic=z, p.value=p)
    # construct object
    out <- list(statistics=stat, test=test, variables=variables[1:2],
                n=length(d), type="paired")
  } else {
    ## rank sum test
    if (length(variables) == 0) stop("a variable to test must be specified")
    variable <- data[, variables[1]]
    by <- as.factor(data[, group[1]])
    if (nlevels(by) != 2) {
      stop("rank sum test requires exactly two groups")
    }
    ok <- is.finite(variable) & !is.na(by)
    variable <- variable[ok]
    by <- by[ok]
    # compute ranks
    r <- rank(variable)
    # compute statistics
    first <- by == levels(by)[1]
    second <- by == levels(by)[2]
    n <- c(sum(first), sum(second))
    if (any(n == 0)) stop("all differences negative or positive")
    sum <- c(sum(r[first]), sum(r[second]))
    stat <- data.frame(N=n, "Mean Rank"=sum/n, "Sum of Ranks"=sum,
                       check.names=FALSE, row.names=levels(by))
    # normal approximation
    max <- which.max(sum)
    N <- sum(n)
    mu <- n[max]*(N+1) / 2
    nTies <- table(r)
    sigma <- sqrt(prod(n)/12 * ((N+1) - sum(nTies^3 - nTies)/(N*(N-1))))
    z <- (sum[max] - mu) / sigma
    p <- 2 * min(pnorm(z), pnorm(z, lower.tail=FALSE))
    asymptotic <- list(statistic=z, p.value=p)
    # exact test without correction for ties
    u <- sum[max] - n[max]*(n[max]+1)/2
    sigma <- sqrt((prod(n)*(N+1)) / 12)
    if (u > prod(n)/2) p <- pwilcox(u-1, n[max], n[-max], lower.tail=FALSE)
    else p <- pwilcox(u, n[max], n[-max])
    p <- min(2*p, 1)
    exact <- list(statistic=u, p.value=p)
    # construct object
    out <- list(statistics=stat, w=sum[max], asymptotic=asymptotic, exact=exact,
                variables=variables[1], group=group[1], type="independent")
  }
  ## return results
  class(out) <- "wilcoxonTest"
  out
}

#' @export
print.wilcoxonTest <- function(x, digits = 3, statistics = c("ranks", "test"),
                               ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)

  ## print LaTeX table for ranks
  if ("ranks" %in% statistics) {
    formatted <- formatSPSS(x$statistics, digits=2)
    formatted[, "N"] <- paste0(formatted[, "N"], "$^\\text{", c("a", "b"), "}$")
    # print LaTeX table
    cat("\n")
    if (x$type == "paired") {
      cat("\\begin{tabular}{|ll|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{Ranks}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean Rank} & \\multicolumn{1}{|c|}{Sum of Ranks} \\\\\n")
      cat("\\hline\n")
      cat(x$variables[2], "-", x$variables[1], "&", rownames(formatted)[1], "&", paste(formatted[1, ], collapse=" & "), "\\\\\n")
      cat(" &", rownames(formatted)[2], "&", paste(formatted[2, ], collapse=" & "), "\\\\\n")
      cat(" & Ties & ", x$n - sum(x$statistics$N), "$^\\text{c}$ & & \\\\\n", sep="")
      cat(" & Total &", x$n, "& & \\\\\n")
      cat("\\hline\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{l}{", "a. ", x$variables[2], " < ", x$variables[1], "} \\\\\n", sep="")
      cat("\\multicolumn{5}{l}{", "b. ", x$variables[2], " > ", x$variables[1], "} \\\\\n", sep="")
      cat("\\multicolumn{5}{l}{", "c. ", x$variables[2], " = ", x$variables[1], "} \\\\\n", sep="")
    } else if (x$type == "independent") {
      formatted <- formatSPSS(x$statistics, digits=2)
      # print LaTeX table
      cat("\\begin{tabular}{|ll|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{Ranks}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" &", x$group, "& \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean Rank} & \\multicolumn{1}{|c|}{Sum of Ranks} \\\\\n")
      cat("\\hline\n")
      cat(x$variables, "&", rownames(formatted)[1], "&", paste(formatted[1, ], collapse=" & "), "\\\\\n")
      cat(" &", rownames(formatted)[2], "&", paste(formatted[2, ], collapse=" & "), "\\\\\n")
      cat(" & Total &", sum(x$statistics$N), "& & \\\\\n")
      cat("\\hline\\noalign{\\smallskip}\n")
    } else stop("type of test not supported")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {

    ## collect output for test
    if (x$type == "paired") {
      test <- c(x$test$statistic, x$test$p.value)
      formatted <- formatSPSS(test, digits=digits)
      min <- which.min(x$statistics[, "Sum of Ranks"])
    } else if (x$type == "independent") {
      test <- c(x$exact$statistic, x$w, x$asymptotic$statistic,
                x$asymptotic$p.value, x$exact$p.value)
      formatted <- formatSPSS(test, digits=digits)
    } else stop("type of test not supported")

    ## print LaTeX table
    if (count == 0) cat("\n")
    if (x$type == "paired") {
      cat("\\begin{tabular}{|l|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{c}{\\textbf{Test Statistics}$^{\\text{a}}$} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & \\multicolumn{1}{|c|}{", paste(x$variables[2], "-", x$variables[1]), "} \\\\\n", sep="")
      cat("\\hline\n")
      cat("Z & ", formatted[1], "$^\\text{b}$ \\\\\n", sep="")
      cat("Asymp. Sig. (2-tailed) &", formatted[2], "\\\\\n")
      cat("\\hline\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{l}{a. Wilcoxon Signed Ranks Test} \\\\\n")
      cat("\\multicolumn{2}{l}{b. Based on", c("negative", "positive")[min], "ranks.} \\\\\n")
    } else if (x$type == "independent") {
      cat("\\begin{tabular}{|l|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{c}{\\textbf{Test Statistics}$^{\\text{a}}$} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & \\multicolumn{1}{|c|}{", x$variables, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat("Mann-Whitney U &", formatted[1], "\\\\\n")
      cat("Wilcoxon W &", formatted[2], "\\\\\n")
      cat("Z &", formatted[3], "\\\\\n")
      cat("Asymp. Sig. (2-tailed) &", formatted[4], "\\\\\n")
      cat("Exact Sig. [2*(1-tailed Sig.)] &", formatted[5], "$^\\text{b}$ \\\\\n", sep="")
      cat("\\hline\\noalign{\\smallskip}\n")
      cat("\\multicolumn{2}{l}{a. Grouping Variable: ", x$group, "} \\\\\n", sep="")
      cat("\\multicolumn{2}{l}{b. Not corrected for ties.} \\\\\n")
    } else stop("type of test not supported")
    # finalize LaTeX table
    cat("\\end{tabular}\n")
    cat("\n")
  }
}
