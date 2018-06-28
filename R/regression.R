#' @export
regression <- function(..., data, labels = NULL) {
  # initializations
  formulas <- list(...)
  if (is.null(labels)) labels <- seq_along(formulas)
  names(formulas) <- labels
  # check if response is the same in all models
  response <- vapply(formulas, function(x) as.character(x[[2]]), character(1))
  response <- unique(response)
  if (length(response) > 1) {
    stop("the same response must be used for all models")
  }
  # fit linear models
  # FIXME: make sure that the same observations are used for all models
  #        in case of missing values
  models <- lapply(formulas, lm, data=data)
  # return results
  out <- list(models=models, response=response)
  class(out) <- "regression"
  out
}

#' @importFrom stats aggregate anova pf
#' @export
print.regression <- function(x, digits = 3,
                             statistics = c("summary", "anova", "estimates"),
                             ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)
  models <- x$models
  labels <- names(models)

  ## extract coefficients
  summaries <- lapply(models, summary)
  coefficients <- mapply(function(m, s) {
    # extract coefficients
    coef <- coefficients(s)
    # compute standardized coefficients
    syx <- vapply(m$model, sd, numeric(1))
    beta <- c(NA_real_, coef[-1, 1] * syx[-1] / syx[1])
    # rename rows and columns
    rownames(coef)[1] <- "(Constant)"
    colnames(coef) <- c("B", "Std. Error", "t", "Sig.")
    # combine information on coefficients
    cbind(coef[, c("B", "Std. Error")], Beta=beta, coef[, c("t", "Sig.")])
  }, models, summaries, SIMPLIFY=FALSE)
  predictors <- lapply(coefficients, rownames)

  ## compute ANOVA tables
  anovas <- mapply(function(m, s) {
    # perform ANOVA
    a <- anova(m)
    class(a) <- "data.frame"
    # aggregate information from individual variables
    a$Type <- ifelse(row.names(a) == "Residuals", "Residual", "Regression")
    sum <- aggregate(a[, c("Sum Sq", "Df")], a[, "Type", drop=FALSE], sum)
    mean <- aggregate(a[, "Mean Sq", drop=FALSE], a[, "Type", drop=FALSE], mean)
    a <- cbind(sum[, c("Sum Sq", "Df")], mean[, "Mean Sq", drop=FALSE])
    row.names(a) <- sum$Type
    # extract information from test
    f <- s$fstatistic[1]
    p <- pf(f, s$fstatistic[2], s$fstatistic[3], lower.tail=FALSE)
    # add test results
    a[, "F"] <- c(f, NA_real_)
    a[, "Sig."] <- c(p, NA_real_)
    # add total sum of squares
    total <- data.frame("Sum Sq"=sum(a[, "Sum Sq"]),  Df=sum(a[, "Df"]),
                        "Mean Sq"=NA_real_, "F"=NA_real_, "Sig."=NA_real_,
                        row.names="Total", check.names=FALSE)
    # return results
    rbind(a, total)
  }, models, summaries, SIMPLIFY=FALSE)

  ## compute R squares
  rsq <- vapply(summaries, "[[", numeric(1), "r.squared")
  adjrsq <- vapply(summaries, "[[", numeric(1), "adj.r.squared")
  sigma <- vapply(summaries, "[[", numeric(1), "sigma")
  fits <- data.frame("R"=sqrt(rsq), "R Sq"=rsq,
                     "Adj. R Sq"=adjrsq, "Std. Error"=sigma,
                     row.names=labels, check.names=FALSE)

  ## compute change statistics
  k <- length(models)
  # compute R square changes
  rsqchange <- diff(rsq)
  rsqfull <- rsq[-k]
  # compute degrees of freedom
  df <- vapply(anovas, function(a) a$Df[1:2], integer(2), USE.NAMES=FALSE)
  df1 <- abs(diff(df[1, ]))
  df2 <- df[2, -1]
  # compute F changes and p-values
  f <- (abs(rsqchange) / df1) / ((1 - rsqfull) / df2)
  p <- pf(f, df1, df2, lower.tail=FALSE)
  # add information from first model
  first <- anovas[[1]]
  changes <- data.frame("R Sq Change"=c(rsq[1], rsqchange),
                        "F Change"=c(first[1, "F"], f),
                        df1=c(first[1, "Df"], df1),
                        df2=c(first[2, "Df"], df2),
                        "Sig. F Change"=c(first[1, "Sig."], p),
                        row.names=labels, check.names=FALSE)

  ## print LaTeX table for model summaries
  if ("summary" %in% statistics) {
    # initialize LaTeX table
    cat("\n")
    cat("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|r|}\n")
    # print table header
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{10}{c}{\\textbf{Model Summary}} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & & & & \\multicolumn{1}{|c|}{Std. Error} & \\multicolumn{5}{|c|}{Change Statistics} \\\\\n")
    cat("\\cline{6-10}\n")
    cat(" & & & \\multicolumn{1}{|c|}{Adjusted} & \\multicolumn{1}{|c|}{of the} & \\multicolumn{1}{|c|}{R Square} & & & & \\multicolumn{1}{|c|}{Sig. F} \\\\\n")
    cat("\\multicolumn{1}{|c}{Model} & \\multicolumn{1}{|c|}{R} & \\multicolumn{1}{|c|}{R Square} & \\multicolumn{1}{|c|}{R Square} & \\multicolumn{1}{|c|}{Estimate} & \\multicolumn{1}{|c|}{Change} & \\multicolumn{1}{|c|}{F Change} & \\multicolumn{1}{|c|}{df1} & \\multicolumn{1}{|c|}{df2} & \\multicolumn{1}{|c|}{Change} \\\\\n")
    cat("\\hline\n")
    # format model summaries
    formatted <- formatSPSS(cbind(fits, changes), digits=digits)
    for (i in seq_along(models)) {
      # print current model summary
      superscript <- sprintf("$^{\\text{%s}}$", letters[i])
      cat(labels[i], " & ", formatted[i, 1], superscript, " & ",
          paste0(formatted[i, -1], collapse=" & "), " \\\\\n", sep="")
    }
    # finalize LaTeX table
    cat("\\hline\n")
    for (i in seq_along(predictors)) {
      cat("\\multicolumn{10}{l}{", letters[i],". Predictors: ",
          paste0(predictors[[i]], collapse=", "), "} \\\\\n", sep="")
    }
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for ANOVA tables
  if ("anova" %in% statistics) {
    # initialize LaTeX table
    if (count == 0) cat("\n")
    cat("\\begin{tabular}{|ll|r|r|r|r|r|}\n")
    # print table header
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{7}{c}{\\textbf{ANOVA}$^{\\text{a}}$} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & & \\multicolumn{1}{|c|}{Sum of} & & & & \\\\\n")
    cat("\\multicolumn{1}{|c}{Model} & & \\multicolumn{1}{|c|}{Squares} & \\multicolumn{1}{|c|}{df} & \\multicolumn{1}{|c|}{Mean Square} & \\multicolumn{1}{|c|}{F} & \\multicolumn{1}{|c|}{Sig.} \\\\\n")
    cat("\\hline\n")
    for (i in seq_along(anovas)) {
      # extract current ANOVA table
      formatted <- formatSPSS(anovas[[i]], digits=digits)
      # print current ANOVA table
      for (type in rownames(formatted)) {
        if (type == "Regression") {
          label <- labels[i]
          superscript <- sprintf("$^\\text{%s}$", letters[i+1])
        } else {
          label <- NULL
          superscript <- NULL
        }
        cat(label, " & ", type, " & ", paste0(formatted[type, ], collapse=" & "),
            superscript, " \\\\\n", sep="")
      }
      # finalize current ANOVA table
      cat("\\hline\n")
    }
    # finalize LaTeX table
    cat("\\multicolumn{7}{l}{a. Dependent variable: ", x$response,
        "} \\\\\n", sep="")
    for (i in seq_along(predictors)) {
      cat("\\multicolumn{7}{l}{", letters[i+1],". Predictors: ",
          paste0(predictors[[i]], collapse=", "), "} \\\\\n", sep="")
    }
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for coefficients
  if ("estimates" %in% statistics) {
    # initialize LaTeX table
    if (count == 0) cat("\n")
    cat("\\begin{tabular}{|ll|r|r|r|r|r|}\n")
    # print table header
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{7}{c}{\\textbf{Coefficients}$^{\\text{a}}$} \\\\\n")
    cat("\\noalign{\\smallskip}\\hline\n")
    cat(" & & \\multicolumn{2}{|c|}{Unstandardized} & \\multicolumn{1}{|c|}{Standardized} & & \\\\\n")
    cat(" & & \\multicolumn{2}{|c|}{Coefficients} & \\multicolumn{1}{|c|}{Coefficients} & & \\\\\n")
    cat("\\cline{3-5}\n")
    cat("\\multicolumn{1}{|c}{Model} & & \\multicolumn{1}{|c|}{B} & \\multicolumn{1}{|c|}{Std. Error} & \\multicolumn{1}{|c|}{Beta} & \\multicolumn{1}{|c|}{t} & \\multicolumn{1}{|c|}{Sig.} \\\\\n")
    cat("\\hline\n")
    for (i in seq_along(coefficients)) {
      # extract current coefficients
      formatted <- formatSPSS(coefficients[[i]], digits=digits)
      # print current coefficients
      for (variable in rownames(formatted)) {
        cat(if (variable == "(Constant)") labels[i], "&", variable, "&",
            paste0(formatted[variable, ], collapse=" & "), "\\\\\n")
      }
      # finalize current model
      cat("\\hline\n")
    }
    # finalize LaTeX table
    cat("\\multicolumn{7}{l}{a. Dependent variable: ", x$response,
        "} \\\\\n", sep="")
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
  }
}

#' @importFrom stats coef
#' @export
coef.regression <- function(object, ...) {
  nm <- length(object$models)
  coef(object$models[[nm]])
}

#' @importFrom stats df.residual
#' @export
df.residual.regression <- function(object, ...) {
  nm <- length(object$models)
  df.residual(object$models[[nm]])
}

#' @importFrom stats sd
#' @export
fitted.regression <- function(object, standardized = FALSE, ...) {
  # extract fitted values from the last model
  nm <- length(object$models)
  fitted <- fitted(object$models[[nm]])
  # standardize if requested
  if (standardized) fitted <- (fitted - mean(fitted)) / sd(fitted)
  # return (standardized) fitted values
  fitted
}

#' @importFrom stats df.residual
#' @export
residuals.regression <- function(object, standardized = FALSE, ...) {
  # extract residuals from the last model
  nm <- length(object$models)
  residuals <- residuals(object$models[[nm]])
  # standardize if requested
  if (standardized) {
    s <- sqrt(sum(residuals^2) / df.residual(object))
    residuals <- residuals / s
  }
  # return (standardized) residuals
  residuals
}

#' @export
plot.regression <- function(x, y, which = c("histogram", "scatter"),
                            main = NULL, xlab = NULL, ylab = NULL, ...) {
  # initializations
  which <- match.arg(which)
  if (is.null(main)) main <- paste0("Dependent Variable: ", x$response)
  residuals <- residuals(x, standardized=TRUE)
  # histogram
  if (which == "histogram") {
    if (is.null(xlab)) xlab <- "Regression Standardized Residual"
    .hist(residuals, main=main, xlab=xlab, ylab=ylab, ...)
  }
  # histogram
  if (which == "scatter") {
    if (is.null(xlab)) xlab <- "Regression Standardized Predicted Value"
    if (is.null(ylab)) ylab <- "Regression Standardized Residual"
    fitted <- fitted(x, standardized=TRUE)
    .plot(fitted, residuals, main=main, xlab=xlab, ylab=ylab, ...)
  }
}
