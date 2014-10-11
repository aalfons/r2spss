#' @importFrom lawstat levene.test
#' @export
ANOVA <- function(data, variable, group, conf.level = 0.95) {
  ## initializations
  data <- as.data.frame(data)
  variable <- as.character(variable)
  group <- as.character(group)
  if (length(variable) == 0) stop("a variable to test must be specified")
  if (length(group) == 0) stop("a grouping variable must be specified")
  ## select type of ANOVA
  x <- data[, variable[1]]
  if (length(group) == 1) {
    ## one-way ANOVA
    by <- as.factor(data[, group[1]])
    if (nlevels(by) < 2) {
      stop("one-way ANOVA requires at least two groups")
    }
    ok <- is.finite(x) & !is.na(by)
    x <- x[ok]
    by <- by[ok]
    # compute descriptives
    n <- tapply(x, by, length)
    if (any(is.na(n))) stop("unused factor levels")
    n <- c(n, Total=sum(n))
    mean <- c(tapply(x, by, mean), Total=mean(x))
    sd <- c(tapply(x, by, sd), Total=sd(x))
    se <- sd / sqrt(n)
    alpha <- 1 - conf.level
    q <- sapply(n-1, function(df) qt(1 - alpha/2, df=df))
    lower <- mean - q * se
    upper <- mean + q * se
    min <- c(tapply(x, by, min), Total=min(x))
    max <- c(tapply(x, by, max), Total=max(x))
    desc <- data.frame(N=n, Mean=mean, "Std. Deviation"=sd, "Std. Error"=se,
                       "Lower Bound"=lower, "Upper Bound"=upper, Minumum=min,
                       Maximum=max, check.names=FALSE)
    # test variances
    levene <- levene.test(x, by, location="mean")
    # perform ANOVA
    formula <- as.formula(paste(variable[1], "~", group[1]))
    fit <- aov(formula, data=data[ok,])
    test <- summary(fit)
    # construct object
    out <- list(descriptives=desc, variance=levene, test=test,
                variable=variable[1], group=group[1],
                conf.level=conf.level, type="one-way")
  } else {
    ## two-way ANVOA

  }
  ## return results
  class(out) <- "ANOVA"
  out
}

#' @export
print.ANOVA <- function(x, digits = 3,
                        statistics = c("descriptives", "variance", "test"),
                        ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)

  ## extract Levene test and ANOVA table
  if (x$type == "one-way") {
    test <- x$test[[1]][, c(2, 1, 3:5)]
    test$Df <- as.integer(test$Df)
    rownames(test) <- c("Between Groups", "Within Groups")
    levene <- data.frame("Levene Statistic"=x$variance$statistic, df1=test$Df[1] ,
                         df2=test$Df[2], "Sig."=x$variance$p.value,
                         check.names=FALSE, row.names=NULL)
  } else if (x$type == "two-way") {

  } else stop("type of ANOVA not supported")

  ## print LaTeX table for descriptives
  if ("descriptives" %in% statistics) {
    cat("\n")
    formatted <- formatSPSS(x$descriptives, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      # initialize LaTeX table
      cat("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{9}{c}{\\textbf{Descriptives}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{9}{l}{", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat(" & & & & & \\multicolumn{2}{|c|}{", format(100*x$conf.level, digits=digits), "\\% Confidence} & & \\\\\n", sep="")
      cat(" & & & & & \\multicolumn{2}{|c|}{Interval for Mean} & & \\\\\n")
      cat("\\cline{6-7}\n")
      cat(" & & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} & & \\\\\n")
      cat(" & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{Error} & \\multicolumn{1}{|c|}{Bound} & \\multicolumn{1}{|c|}{Bound} & \\multicolumn{1}{|c|}{Minimum} & \\multicolumn{1}{|c|}{Maximum} \\\\\n")
      cat("\\hline\n")
      # print table
      for (rn in rownames(formatted)) {
        cat(rn, "&", paste(formatted[rn, ], collapse=" & "), "\\\\\n")
      }
    } else if (x$type == "two-way") {

    } else stop("type of ANOVA not supported")
    # finalize LaTeX table
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for Levene test
  if ("variance" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    formatted <- formatSPSS(levene, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      # initialize LaTeX table
      cat("\\begin{tabular}{|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{4}{c}{\\textbf{Test of Homogeneity of Variances}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{4}{l}{", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat("\\multicolumn{1}{|c|}{Levene Statistic} & \\multicolumn{1}{|c|}{df1} & \\multicolumn{1}{|c|}{df2} & \\multicolumn{1}{|c|}{Sig.} \\\\\n")
      cat("\\hline\n")
      # print table
      cat(paste(formatted, collapse=" & "), "\\\\\n")
    } else if (x$type == "two-way") {

    } else stop("type of ANOVA not supported")
    # finalize LaTeX table
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
  }

  ## print LaTeX table for ANOVA table
  if ("test" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    formatted <- formatSPSS(test, digits=digits)
    # print LaTeX table
    if (x$type == "one-way") {
      cat("\\begin{tabular}{|l|r|r|r|r|r|}\n")
      # print table header
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{c}{\\textbf{ANOVA}} \\\\\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{l}{", x$variable, "} \\\\\n", sep="")
      cat("\\hline\n")
      cat(" & \\multicolumn{1}{|c|}{Sum of Squares} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{Mean Square} & \\multicolumn{1}{|c|}{F} & \\multicolumn{1}{|c|}{Sig.} \\\\\n")
      cat("\\hline\n")
      # print table
      for (rn in rownames(formatted)) {
        cat(rn, "&", paste(formatted[rn, ], collapse=" & "), "\\\\\n")
      }
      cat("Total &", formatSPSS(sum(test[, "Sum Sq"]), digits=digits), "&", sum(test$Df), "& & & \\\\\n")
    } else if (x$type == "two-way") {

    } else stop("type of ANOVA not supported")
    # finalize LaTeX table
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
  }
}

#' @export
plot.ANOVA <- function(x, y, main = NULL, xlab = NULL, ylab = NULL,
                       type = "o", ...) {
  if (x$type == "one-way") {
    if (is.null(xlab)) xlab <- x$group
    if (is.null(ylab)) ylab <- paste("Mean of", x$variable)
    desc <- x$descriptives
    n <- nrow(x$descriptives)
    labs <- row.names(x$descriptives)[-n]
    means <- x$descriptives$Mean[-n]
    .lines(labs, means, type=type, main=main, xlab=xlab, ylab=ylab, ...)
  } else if (x$type == "two-way") {

  } else stop("type of ANOVA not supported")
}
