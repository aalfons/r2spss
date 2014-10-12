#' @importFrom car leveneTest
#' @export
tTest <- function(data, variables, group = NULL, mu = 0, conf.level = 0.95) {
  ## initializations
  data <- as.data.frame(data)
  variables <- as.character(variables)
  group <- as.character(group)
  if (length(variables) == 0) stop("a variable to test must be specified")
  ## select t test
  if (length(group) == 0) {
    if (length(variables) == 1) {
      ## one-sample t test
      x <- data[, variables[1]]
      # compute statistics
      stat <- .stat(x)
      row.names(stat) <- variables[1]
      # perform test
      ok <- is.finite(x)
      x <- x[ok]
      test <- t.test(x, mu=mu, conf.level=conf.level)
      # construct object
      out <- list(statistics=stat, test=test, variables=variables[1],
                  type="one-sample")
    } else {
      ## paired-samples t test
      x <- data[, variables[1]]
      y <- data[, variables[2]]
      # compute statistics
      stat <- rbind(.stat(x), .stat(y))
      row.names(stat) <- variables[1:2]
      # perform test
      ok <- is.finite(x) & is.finite(y)
      x <- x[ok]
      y <- y[ok]
      test <- t.test(x, y, mu=0, paired=TRUE, conf.level=conf.level)
      # construct object
      out <- list(statistics=stat, test=test, variables=variables[1:2],
                  n=length(x), type="paired")
    }
  } else {
    ## independent-samples t test
    variable <- data[, variables[1]]
    by <- as.factor(data[, group[1]])
    if (nlevels(by) != 2) {
      stop("independent-samples t test requires exactly two groups")
    }
    ok <- is.finite(variable) & !is.na(by)
    variable <- variable[ok]
    by <- by[ok]
    # perform tests
    levene <- leveneTest(variable, by, center="mean")
    x <- variable[by == levels(by)[1]]
    y <- variable[by == levels(by)[2]]
    pooled <- t.test(x, y, mu=0, paired=FALSE, var.equal=TRUE,
                     conf.level=conf.level)
    satterthwaite <- t.test(x, y, mu=0, paired=FALSE, var.equal=FALSE,
                            conf.level=conf.level)
    # compute statistics
    stat <- rbind(.stat(x), .stat(y))
    row.names(stat) <- levels(by)
    # construct object
    out <- list(statistics=stat, levene=levene, pooled=pooled,
                satterthwaite=satterthwaite, variables=variables[1],
                group=group[1], type="independent")
  }
  ## return results
  class(out) <- "tTest"
  out
}

# compute mean, standard deviation and standard error
.stat <- function(x) {
  # initializations
  ok <- is.finite(x)
  x <- x[ok]
  # compute statistics
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  se <- sd / sqrt(n)
  # return data frame
  data.frame(N=n, Mean=mean, "Std. Deviation"=sd, "Std. Error Mean"=se,
             check.names=FALSE)
}

#' @export
print.tTest <- function(x, digits = 3, statistics = c("statistics", "test"),
                        ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)

  ## print LaTeX table for statistics
  if ("statistics" %in% statistics) {
    formatted <- formatSPSS(x$statistics, digits=digits)
    # print LaTeX table
    cat("\n")
    if (x$type == "one-sample") {
      cat("\\begin{tabular}{|l|r|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{One-Sample Statistics}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Std. Error} \\\\\n")
      cat(" & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{Mean} \\\\\n")
      cat("\\hline\n")
      cat(rownames(formatted), "&", paste0(formatted, collapse=" & "), "\\\\\n")
    } else if (x$type == "paired") {
      cat("\\begin{tabular}{|l|r|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{5}{c}{\\textbf{Paired Samples Statistics}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Std. Error} \\\\\n")
      cat(" & \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{Mean} \\\\\n")
      cat("\\hline\n")
      cat(rownames(formatted)[1], "&", paste0(formatted[1, ], collapse=" & "), "\\\\\n")
      cat(rownames(formatted)[2], "&", paste0(formatted[2, ], collapse=" & "), "\\\\\n")
    } else if (x$type == "independent") {
      cat("\\begin{tabular}{|ll|r|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{6}{c}{\\textbf{Group Statistics}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Std. Error} \\\\\n")
      cat(" &", x$group, "& \\multicolumn{1}{|c|}{N} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{Mean} \\\\\n")
      cat("\\hline\n")
      cat(x$variables, "&", rownames(formatted)[1], "&", paste0(formatted[1, ], collapse=" & "), "\\\\\n")
      cat(" &", rownames(formatted)[2], "&", paste0(formatted[2, ], collapse=" & "), "\\\\\n")
    } else stop("type of test not supported")
    # finalize LaTeX table
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for test
  if ("test" %in% statistics) {

    ## collect output for test
    if (x$type == "one-sample") {
      est <- x$test$estimate
      mu <- x$test$null.value
      ci <- x$test$conf.int - mu
      gamma <- attr(ci, "conf.level")
      test <- data.frame(t=x$test$statistic, df=as.integer(x$test$parameter),
                         "Sig."=x$test$p.value, "Mean Difference"=est-mu,
                         Lower=ci[1], Upper=ci[2], check.names=FALSE,
                         row.names=x$variables)
      formatted <- formatSPSS(test, digits=digits)
    } else if (x$type == "paired") {
      df <- as.integer(x$test$parameter)
      ci <- x$test$conf.int
      gamma <- attr(ci, "conf.level")
      alpha <- 1 - gamma
      se <- diff(ci) / (2 * qt(1-alpha/2, df=df))
      sd <- se * sqrt(x$n)
      rn <- paste0(x$variables, collapse=" - ")
      test <- data.frame(Mean=x$test$estimate, "Std. Deviation"=sd,
                         "Std. Error Mean"=se, Lower=ci[1], Upper=ci[2],
                         t=x$test$statistic, df=df, "Sig."=x$test$p.value,
                         check.names=FALSE, row.names=rn)
      formatted <- formatSPSS(test, digits=digits)
    } else if (x$type == "independent") {
      levene <- data.frame("F"=x$levene[, "F value"],
                           "Sig."=x$levene[, "Pr(>F)"],
                           check.names=FALSE, row.names=NULL)
      # equal variances assumed
      est <- x$pooled$estimate[1] - x$pooled$estimate[2]
      df <- as.integer(x$pooled$parameter)
      ci <- x$pooled$conf.int
      gamma <- attr(ci, "conf.level")
      alpha <- 1 - gamma
      se <- diff(ci) / (2 * qt(1-alpha/2, df=df))
      pooled <- data.frame(t=x$pooled$statistic, df=df, "Sig."=x$pooled$p.value,
                           "Mean Difference"=est, "Std. Error Difference"=se,
                           Lower=ci[1], Upper=ci[2], check.names=FALSE,
                           row.names=NULL)
      # equal variances not assumed
      est <- x$satterthwaite$estimate[1] - x$satterthwaite$estimate[2]
      df <- x$satterthwaite$parameter
      ci <- x$satterthwaite$conf.int
      gamma <- attr(ci, "conf.level")
      alpha <- 1 - gamma
      se <- diff(ci) / (2 * qt(1-alpha/2, df=df))
      satterthwaite <- data.frame(t=x$satterthwaite$statistic, df=df,
                                  "Sig."=x$satterthwaite$p.value,
                                  "Mean Difference"=est,
                                  "Std. Error Difference"=se,
                                  Lower=ci[1], Upper=ci[2],
                                  check.names=FALSE, row.names=NULL)
      # combine tests
      formatted <- rbind(formatSPSS(pooled, digits=digits),
                         formatSPSS(satterthwaite, digits=digits))
      formatted <- cbind(formatSPSS(levene, digits=digits), formatted)
    } else stop("type of test not supported")

    ## print LaTeX table
    if (count == 0) cat("\n")
    if (x$type == "one-sample") {
      cat("\\begin{tabular}{|l|r|r|r|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{7}{c}{\\textbf{One-Sample Test}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & \\multicolumn{6}{|c|}{Test Value = ", format(x$test$null.value, digits=digits), "} \\\\\n", sep="")
      cat("\\cline{2-7}\n")
      cat(" & & & & & \\multicolumn{2}{|c|}{", format(100*gamma, digits=digits), "\\% Confidence} \\\\\n", sep="")
      cat(" & & & & & \\multicolumn{2}{|c|}{Interval of the} \\\\\n")
      cat(" & & & \\multicolumn{1}{|c|}{Sig. (2-} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{2}{|c|}{Difference} \\\\\n")
      cat("\\cline{6-7}\n")
      cat(" & \\multicolumn{1}{|c|}{t} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{tailed)} & \\multicolumn{1}{|c|}{Difference} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} \\\\\n")
      cat("\\hline\n")
      cat(rownames(formatted), "&", paste0(formatted, collapse=" & "), "\\\\\n")
    } else if (x$type == "paired") {
      cat("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{9}{c}{\\textbf{Paired Samples Test}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & \\multicolumn{5}{|c|}{Paired Differences} & & & \\\\\n", sep="")
      cat("\\cline{2-6}\n")
      cat(" & & & & \\multicolumn{2}{|c|}{", format(100*gamma, digits=digits), "\\% Confidence} & & & \\\\\n", sep="")
      cat(" & & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{2}{|c|}{Interval of the} & & & \\\\\n")
      cat(" & & \\multicolumn{1}{|c|}{Std.} & \\multicolumn{1}{|c|}{Error} & \\multicolumn{2}{|c|}{Difference} & & & \\multicolumn{1}{|c|}{Sig. (2-} \\\\\n")
      cat("\\cline{5-6}\n")
      cat(" & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Deviation} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} & \\multicolumn{1}{|c|}{t} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{tailed)} \\\\\n")
      cat("\\hline\n")
      cat(rownames(formatted), "&", paste0(formatted, collapse=" & "), "\\\\\n")
    } else if (x$type == "independent") {
      cat("\\begin{tabular}{|ll|r|r|r|r|r|r|r|r|r|}\n")
      cat("\\noalign{\\smallskip}\n")
      cat("\\multicolumn{11}{c}{\\textbf{Independent Samples Test}} \\\\\n")
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & \\multicolumn{2}{|c|}{Levene's Test} & \\multicolumn{7}{|c|}{} \\\\\n", sep="")
      cat(" & & \\multicolumn{2}{|c|}{for Equality} & \\multicolumn{7}{|c|}{} \\\\\n", sep="")
      cat(" & & \\multicolumn{2}{|c|}{of Variances} & \\multicolumn{7}{|c|}{t-test for Equality of Means} \\\\\n", sep="")
      cat("\\cline{3-11}\n")
      cat(" & & & & & & & & & \\multicolumn{2}{|c|}{", format(100*gamma, digits=digits), "\\% Confidence} \\\\\n", sep="")
      cat(" & & & & & & & & & \\multicolumn{2}{|c|}{Interval of the} \\\\\n")
      cat(" & & & & & & \\multicolumn{1}{|c|}{Sig. (2-} & \\multicolumn{1}{|c|}{Mean} & \\multicolumn{1}{|c|}{Std. Error} & \\multicolumn{2}{|c|}{Difference} \\\\\n")
      cat("\\cline{10-11}\n")
      cat(" & & \\multicolumn{1}{|c|}{F} & \\multicolumn{1}{|c|}{Sig.} & \\multicolumn{1}{|c|}{t} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{tailed)} & \\multicolumn{1}{|c|}{Difference} & \\multicolumn{1}{|c|}{Difference} & \\multicolumn{1}{|c|}{Lower} & \\multicolumn{1}{|c|}{Upper} \\\\\n")
      cat("\\hline\n")
      cat(x$variables, "& Equal &", paste0(formatted[1,], collapse=" & "), "\\\\\n")
      cat(" & variances & & & & & & & & & \\\\\n")
      cat(" & assumed & & & & & & & & & \\\\\n")
      cat("\\hline\n")
      cat(" & Equal &", paste0(formatted[2,], collapse=" & "), "\\\\\n")
      cat(" & variances & & & & & & & & & \\\\\n")
      cat(" & not & & & & & & & & & \\\\\n")
      cat(" & assumed & & & & & & & & & \\\\\n")
    } else stop("type of test not supported")
    # finalize LaTeX table
    cat("\\hline\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
  }
}
