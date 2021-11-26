# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Linear Regression
#'
#' Perform linear regression on variables of a data set.  The output is printed
#' as a LaTeX table that mimics the look of SPSS output (version <24), and plots
#' of the results mimic the look of SPSS graphs.
#'
#' @param \dots  for \code{regression}, at least one formula specifying a
#' regression model.  Different models can be compared by supplying multiple
#' formulas.  For the \code{plot} method, additional arguments to be passed
#' down, in particular graphical parameters (see also \code{\link{histSPSS}}
#' and \code{\link{plotSPSS}}).  For other methods, this is currently ignored.
#' @param data  a data frame containing the variables.
#' @param labels  a character or numeric vector giving labels for the
#' regression models in the output tables.
#' @param change  a logical indicating whether tests on the
#' \eqn{R^2}{R-squared} change should be included in model summaries.
#'
#' @return  An object of class \code{"regressionSPSS"} with the following
#' components:
#' \describe{
#'   \item{\code{models}}{a list in which each component is an ojbect of class
#'   \code{"lm"} as returned by function \code{\link[stats]{lm}}.}
#'   \item{\code{response}}{a character string containing the name of the
#'   response variable.}
#'   \item{\code{method}}{a character string specifying whether the nested
#'   models are increasing in dimension by entering additional variables
#'   (\code{"enter"}) or decreasing in dimension by removing variables
#'   (\code{"remove"}).}
#'   \item{\code{change}}{a logical indicating whether tests on the
#'   \eqn{R^2}{R-squared} change are included in model summaries.}
#' }
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output (version <24).
#'
#' The \code{coef}, \code{df.residual}, \code{fitted} and \code{residuals}
#' methods return the coefficients, residual degrees of freedom, fitted
#' values and residuals, respectively, of the \emph{last} model (to mimic
#' SPSS functionality).
#'
#' Similarly, the \code{plot} method creates the specified plot for the
#' \emph{last} model.
#'
#' @author Andreas Alfons
#'
#' @examples
#' # load data
#' data("Eredivisie")
#' # log-transform market values
#' Eredivisie$logMarketValue <- log(Eredivisie$MarketValue)
#' # squared values of age
#' Eredivisie$AgeSq <- Eredivisie$Age^2
#'
#' # simple regression model of log market value on age
#' fit1 <- regression(logMarketValue ~ Age, data = Eredivisie)
#' fit1                           # print LaTeX table
#' plot(fit1, which = "scatter")  # diagnostic plot
#'
#' # add a squared effect for age
#' fit2 <- regression(logMarketValue ~ Age + AgeSq,
#'                    data = Eredivisie, labels = 2)
#' fit2                           # print LaTeX table
#' plot(fit2, which = "scatter")  # diagnostic plot
#'
#' # more complex models with model comparison
#' fit3 <- regression(logMarketValue ~ Age + AgeSq,
#'                    logMarketValue ~ Age + AgeSq + Contract +
#'                                     Foreign,
#'                    logMarketValue ~ Age + AgeSq + Contract +
#'                                     Foreign + Position,
#'                    data = Eredivisie, labels = 2:4,
#'                    change = TRUE)
#' fit3                             # print LaTeX table
#' plot(fit3, which = "histogram")  # diagnostic plot
#'
#' @keywords multivariate
#'
#' @importFrom stats lm model.frame na.pass
#' @export

regression <- function(..., data, labels = NULL, change = FALSE) {
  # initializations
  formulas <- list(...)
  if (is.null(labels)) labels <- seq_along(formulas)
  names(formulas) <- labels
  nFormulas <- length(formulas)
  change <- isTRUE(change)
  # check if response is the same in all models
  response <- vapply(formulas, function(x) as.character(x[[2]]), character(1))
  response <- unique(response)
  if (length(response) > 1) {
    stop("the same response must be used for all models")
  }
  # extract response and predictor matrix for each formula
  xyList <- lapply(formulas, function(f) {
    mf <- model.frame(f, data = data, na.action = na.pass,
                      drop.unused.levels = TRUE)
    terms <- attr(mf, "terms")
    list(x = model.matrix(terms, data = mf), y = model.response(mf),
         intercept = attr(terms, "intercept"))
  })
  # check whether all formulas include an intercept
  haveIntercept <- vapply(xyList, function(xy) xy$intercept == 1, logical(1))
  if (!all(haveIntercept)) stop("all models must have an intercept")
  # check whether models are nested and store index of largest model
  if (nFormulas == 1) {
    largest <- 1
    method <- "enter"
  } else {
    # check if we have a forward series (adding variables) or backward series
    # (removing variables) of nested models
    varNames <- lapply(xyList, function(xy) colnames(xy$x))
    forwards <- backwards <- rep.int(NA, nFormulas - 1)
    for (i in seq(nFormulas - 1)) {
      forwards[i] <- (length(varNames[[i]]) < length(varNames[[i+1]])) &&
        all(varNames[[i]] %in% varNames[[i+1]])
      backwards[i] <- (length(varNames[[i+1]]) < length(varNames[[i]])) &&
        all(varNames[[i+1]] %in% varNames[[i]])
    }
    if (!all(forwards) && !all(backwards)) {
      stop("you must specify a series of nested models")
    }
    # store index of largest model
    if (all(forwards)) {
      largest <- nFormulas
      method <- "enter"
    } else {
      largest <- 1
      method = "remove"
    }
  }
  # remove observations with missing values in the largest model
  yx <- cbind(xyList[[largest]]$y, xyList[[largest]]$x)
  names(yx)[1] <- response
  keep <- complete.cases(yx)
  # fit linear models
  models <- lapply(formulas, lm, data=data[keep, ])
  # return results
  out <- list(models=models, response=response, method=method, change=change)
  class(out) <- "regressionSPSS"
  out
}


#' @rdname regression
#'
#' @param x,object  an object of class \code{"regressionSPSS"} as returned by
#' function \code{regression}.
#' @param digits  an integer giving the number of digits after the comma to be
#' printed in the LaTeX tables.
#' @param statistics  a character vector specifying which LaTeX tables should
#' be printed.  Available options are \code{"summary"} for model summaries,
#' \code{"anova"} for ANOVA results, and \code{"estimates"} for estimated
#' coefficients.  The default is to print all tables.
#'
#' @importFrom stats aggregate anova model.matrix model.response pf
#' @export

print.regressionSPSS <- function(x, digits = 3,
                                 statistics = c("summary", "anova", "estimates"),
                                 theme = c("modern", "legacy"), ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok=TRUE)
  theme <- match.arg(theme)
  legacy <- theme == "legacy"
  models <- x$models
  labels <- names(models)
  change <- x$change
  # TODO: it's probably a good idea to let the user supply character limits
  if (change) wrap <- c(90, 66)
  else wrap <- c(50, 66)

  ## extract coefficients
  summaries <- lapply(models, summary)
  coefficients <- mapply(function(m, s) {
    # extract coefficients
    coef <- coefficients(s)
    # extract response and predictor matrix (without intercept column)
    y <- model.response(m$model)
    terms <- attr(m$model, "terms")
    x <- model.matrix(terms, data = m$model)[, -1, drop = FALSE]
    # compute standardized coefficients
    sy <- sd(y)
    sx <- apply(x, 2, sd)
    beta <- c(NA_real_, coef[-1, 1] * sx / sy)
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

  ## if requested, compute change statistics
  if (change) {
    k <- length(models)
    # compute R square changes and degrees of freedom
    rsqchange <- diff(rsq)
    df <- vapply(anovas, function(a) a$Df[1:2], integer(2), USE.NAMES=FALSE)
    if (x$method == "enter") {
      # R square of the full model
      rsqfull <- rsq[-1]
      # compute degrees of freedom
      df1 <- diff(df[1, ])
      df2 <- df[2, -1]
    } else {
      # R square of the full model
      rsqfull <- rsq[-k]
      # compute degrees of freedom
      df1 <- -diff(df[1, ])
      df2 <- df[2, -k]
    }
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
    # add to information on model fits
    fits <- cbind(fits, changes)
  }

  ## print LaTeX table for model summaries
  if ("summary" %in% statistics) {
    # initialize LaTeX table
    cat("\n")
    if (legacy) {
      if (change) cat("\\begin{tabular}{|l|r|r|r|r|r|r|r|r|r|}\n")
      else cat("\\begin{tabular}{|l|r|r|r|r|}\n")
    } else {
      if (change) cat(latexTabular(info = 1, results = 9))
      else cat(latexTabular(info = 1, results = 4))
      cat("\n")
    }
    # print table header
    cat("\\noalign{\\smallskip}\n")
    if (change) cat("\\multicolumn{10}{c}{\\textbf{Model Summary}} \\\\\n")
    else cat("\\multicolumn{5}{c}{\\textbf{Model Summary}} \\\\\n")
    if (legacy) {
      cat("\\noalign{\\smallskip}\\hline\n")
      if (change) {
        cat(" & & & & \\multicolumn{1}{c|}{Std. Error} & \\multicolumn{5}{c|}{Change Statistics} \\\\\n")
        cat("\\cline{6-10}\n")
        cat(" & & & \\multicolumn{1}{c|}{Adjusted} & \\multicolumn{1}{c|}{of the} & \\multicolumn{1}{c|}{R Square} & & & & \\multicolumn{1}{c|}{Sig. F} \\\\\n")
        cat("\\multicolumn{1}{|c|}{Model} & \\multicolumn{1}{c|}{R} & \\multicolumn{1}{c|}{R Square} & \\multicolumn{1}{c|}{R Square} & \\multicolumn{1}{c|}{Estimate} & \\multicolumn{1}{c|}{Change} & \\multicolumn{1}{c|}{F Change} & \\multicolumn{1}{c|}{df1} & \\multicolumn{1}{c|}{df2} & \\multicolumn{1}{c|}{Change} \\\\\n")
      } else {
        cat(" & & & \\multicolumn{1}{c|}{Adjusted} & \\multicolumn{1}{c|}{Std. Error of} \\\\\n")
        cat("\\multicolumn{1}{|c|}{Model} & \\multicolumn{1}{c|}{R} & \\multicolumn{1}{c|}{R Square} & \\multicolumn{1}{c|}{R Square} & \\multicolumn{1}{c|}{the Estimate} \\\\\n")
      }
    } else {
      cat("\\noalign{\\smallskip}\n")
      if (change) {
        cat(latexMulticolumn("", 1), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("Std. Error", 1, right = TRUE), "&",
            latexMulticolumn("Change Statistics", 5), "\\\\\n")
        cat(latexMulticolumn("", 1), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("Adjusted", 1, right = TRUE), "&",
            latexMulticolumn("of the", 1, right = TRUE), "&",
            latexMulticolumn("R Square", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("Sig. F", 1), "\\\\\n")
        cat(latexMulticolumn("Model", 1, "l"), "&",
            latexMulticolumn("R", 1, right = TRUE), "&",
            latexMulticolumn("R Square", 1, right = TRUE), "&",
            latexMulticolumn("R Square", 1, right = TRUE), "&",
            latexMulticolumn("Estimate", 1, right = TRUE), "&",
            latexMulticolumn("Change", 1, right = TRUE), "&",
            latexMulticolumn("F Change", 1, right = TRUE), "&",
            latexMulticolumn("df1", 1, right = TRUE), "&",
            latexMulticolumn("df2", 1, right = TRUE), "&",
            latexMulticolumn("Change", 1), "\\\\\n")
      } else {
        cat(latexMulticolumn("", 1), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("", 1, right = TRUE), "&",
            latexMulticolumn("Adjusted", 1, right = TRUE), "&",
            latexMulticolumn("Std. Error of", 1), "\\\\\n")
        cat(latexMulticolumn("Model", 1, "l"), "&",
            latexMulticolumn("R", 1, right = TRUE), "&",
            latexMulticolumn("R Square", 1, right = TRUE), "&",
            latexMulticolumn("R Square", 1, right = TRUE), "&",
            latexMulticolumn("the Estimate", 1), "\\\\\n")
      }
    }
    cat("\\hline\n")
    # format model summaries
    if (legacy) formatted <- formatSPSS(fits, digits=digits, pValue=FALSE)
    else formatted <- formatSPSS(fits, digits=digits)
    for (i in seq_along(models)) {
      # print current model summary
      superscript <- sprintf("$^{\\text{%s}}$", letters[i])
      cat(labels[i], " & ", formatted[i, 1], superscript, " & ",
          paste0(formatted[i, -1], collapse=" & "), " \\\\\n", sep="")
    }
    # finalize LaTeX table
    cat("\\hline\n")
    for (i in seq_along(predictors)) {
      if (change) {
        catPredictors(predictors[[i]], columns = 10, index = i,
                      wrap = wrap[1])
      } else {
        catPredictors(predictors[[i]], columns = 5, index = i,
                      wrap = wrap[1])
      }
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
    if (legacy) cat("\\begin{tabular}{|ll|r|r|r|r|r|}\n")
    else {
      cat(latexTabular(info = 2, results = 5))
      cat("\n")
    }
    # print table header
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{7}{c}{\\textbf{ANOVA}$^{\\text{a}}$} \\\\\n")
    if (legacy) {
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & \\multicolumn{1}{c|}{Sum of} & & & & \\\\\n")
      cat("\\multicolumn{1}{|c}{Model} & & \\multicolumn{1}{c|}{Squares} & \\multicolumn{1}{c|}{df} & \\multicolumn{1}{c|}{Mean Square} & \\multicolumn{1}{c|}{F} & \\multicolumn{1}{c|}{Sig.} \\\\\n")
    } else {
      cat("\\noalign{\\smallskip}\n")
      cat(latexMulticolumn("", 2), "&",
          latexMulticolumn("Sum of", 1, right = TRUE), "&",
          latexMulticolumn("", 1, right = TRUE), "&",
          latexMulticolumn("", 1, right = TRUE), "&",
          latexMulticolumn("", 1, right = TRUE), "&",
          latexMulticolumn("", 1), "\\\\\n")
      cat(latexMulticolumn("Model", 1, "l"), "&",
          latexMulticolumn("", 1), "&",
          latexMulticolumn("Squares", 1, right = TRUE), "&",
          latexMulticolumn("df", 1, right = TRUE), "&",
          latexMulticolumn("Mean Square", 1, right = TRUE), "&",
          latexMulticolumn("F", 1, right = TRUE), "&",
          latexMulticolumn("Sig.", 1), "\\\\\n")
    }
    cat("\\hline\n")
    for (i in seq_along(anovas)) {
      # extract current ANOVA table
      if (legacy) {
        formatted <- formatSPSS(anovas[[i]], digits=digits, pValue=FALSE)
      } else formatted <- formatSPSS(anovas[[i]], digits=digits)
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
    catResponse(x$response, columns = 7)
    for (i in seq_along(predictors)) {
      catPredictors(predictors[[i]], columns = 7, index = i+1,
                    wrap = wrap[2])
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
    if (legacy) cat("\\begin{tabular}{|ll|r|r|r|r|r|}\n")
    else {
      cat(latexTabular(info = 2, results = 5))
      cat("\n")
    }
    # print table header
    cat("\\noalign{\\smallskip}\n")
    cat("\\multicolumn{7}{c}{\\textbf{Coefficients}$^{\\text{a}}$} \\\\\n")
    if (legacy) {
      cat("\\noalign{\\smallskip}\\hline\n")
      cat(" & & \\multicolumn{2}{c|}{Unstandardized} & \\multicolumn{1}{c|}{Standardized} & & \\\\\n")
      cat(" & & \\multicolumn{2}{c|}{Coefficients} & \\multicolumn{1}{c|}{Coefficients} & & \\\\\n")
      cat("\\cline{3-5}\n")
      cat("\\multicolumn{1}{|c}{Model} & & \\multicolumn{1}{c|}{B} & \\multicolumn{1}{c|}{Std. Error} & \\multicolumn{1}{c|}{Beta} & \\multicolumn{1}{c|}{t} & \\multicolumn{1}{c|}{Sig.} \\\\\n")
    } else {
      cat("\\noalign{\\smallskip}\n")
      cat(latexMulticolumn("", 2), "&",
          latexMulticolumn("Unstandardized", 2, right = TRUE), "&",
          latexMulticolumn("Standardized", 1, right = TRUE), "&",
          latexMulticolumn("", 1, right = TRUE), "&",
          latexMulticolumn("", 1), "\\\\\n")
      cat(latexMulticolumn("", 2), "&",
          latexMulticolumn("Coefficients", 2, right = TRUE), "&",
          latexMulticolumn("Coefficients", 1, right = TRUE), "&",
          latexMulticolumn("", 1, right = TRUE), "&",
          latexMulticolumn("", 1), "\\\\\n")
      cat(latexMulticolumn("Model", 1, "l"), "&",
          latexMulticolumn("", 1), "&",
          latexMulticolumn("B", 1, right = TRUE), "&",
          latexMulticolumn("Std. Error", 1, right = TRUE), "&",
          latexMulticolumn("Beta", 1, right = TRUE), "&",
          latexMulticolumn("t", 1, right = TRUE), "&",
          latexMulticolumn("Sig.", 1), "\\\\\n")
    }
    cat("\\hline\n")
    for (i in seq_along(coefficients)) {
      # extract current coefficients
      if (legacy) {
        formatted <- formatSPSS(coefficients[[i]], digits=digits, pValue=FALSE)
      } else formatted <- formatSPSS(coefficients[[i]], digits=digits)
      # print current coefficients
      for (variable in rownames(formatted)) {
        cat(if (variable == "(Constant)") labels[i], "&", variable, "&",
            paste0(formatted[variable, ], collapse=" & "), "\\\\\n")
      }
      # finalize current model
      # FIXME: line in between models should be in color 'darkgraySPSS'
      cat("\\hline\n")                                     # regular black line
      # cat("\\noalign{\\color{darkgraySPSS}\\hrule}%\n")  # doesn't work
    }
    # finalize LaTeX table
    catResponse(x$response, columns = 7)
    cat("\\noalign{\\smallskip}\n")
    cat("\\end{tabular}\n")
    cat("\n")
  }
}


## utility function to print information on the dependent variable
catResponse <- function(response, columns) {
  cat("\\multicolumn{", columns, "}{l}{a. Dependent variable: ", response,
      "} \\\\\n", sep="")
}

## utility function to print information on the predictors
catPredictors <- function(predictors, columns, index = 1, wrap = 66) {
  # initializations
  p <- length(predictors)
  # prepare string for first predictor
  prefix <- "Predictors: "
  predictorStrings <- paste0("Predictors: ", predictors[1])
  # after each predictor except the last, a separator is inserted
  if (p > 1) {
    separators <- c(rep.int(", ", p-1), "")
    predictorStrings <- paste0(c(predictorStrings, predictors[-1]), separators)
  }
  # add lines with as many predictors as there is space (given by 'wrap')
  i <- 0
  while(length(predictorStrings) > 0) {
    # update number of lines to print
    i <- i + 1
    # determine how many predictors have space
    width <- nchar(predictorStrings)
    add <- which(cumsum(width) <= wrap)
    # too long a variable needs to be added anyway
    if (length(add) == 0) add <- 1
    # print predictors that have
    if (i == 1) {
      cat("\\multicolumn{", columns, "}{l}{", letters[index],". ",
          paste0(predictorStrings[add]), "} \\\\\n", sep="")
    } else {
      cat("\\multicolumn{", columns, "}{l}{\\phantom{", letters[index],". }",
          paste0(predictorStrings[add]), "} \\\\\n", sep="")

    }
    # remove the predictors which have just been printed
    predictorStrings <- predictorStrings[-add]
  }
}


#' @rdname regression
#' @importFrom stats coef
#' @export

coef.regressionSPSS <- function(object, ...) {
  nm <- length(object$models)
  coef(object$models[[nm]])
}


#' @rdname regression
#' @importFrom stats df.residual
#' @export

df.residual.regressionSPSS <- function(object, ...) {
  nm <- length(object$models)
  df.residual(object$models[[nm]])
}


#' @rdname regression
#' @importFrom stats sd
#' @export

fitted.regressionSPSS <- function(object, standardized = FALSE, ...) {
  # extract fitted values from the last model
  nm <- length(object$models)
  fitted <- fitted(object$models[[nm]])
  # standardize if requested
  if (standardized) fitted <- (fitted - mean(fitted)) / sd(fitted)
  # return (standardized) fitted values
  fitted
}


#' @rdname regression
#'
#' @param standardized  a logical indicating whether to return standardized
#' residuals and fitted values (\code{TRUE}), or residuals and fitted values on
#' their original scale (\code{FALSE}).
#'
#' @importFrom stats df.residual
#' @export

residuals.regressionSPSS <- function(object, standardized = FALSE, ...) {
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


#' @rdname regression
#'
#' @param y  ignored (only included because it is defined for the generic
#' function \code{\link[graphics]{plot}}).
#' @param which  a character string specifying which plot to produce.  Possible
#' values are \code{"histogram"} for a histogram of the residuals, or
#' \code{"scatter"} for a scatterplot of the standardized residuals against the
#' standardized fitted values.
#' @param main,xlab,ylab  the plot title and axis labels.
#'
#' @export

plot.regressionSPSS <- function(x, y, which = c("histogram", "scatter"),
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
