# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Linear Regression
#'
#' Perform linear regression on variables of a data set.  The output is printed
#' as a LaTeX table that mimics the look of SPSS output, and plots of the
#' results mimic the look of SPSS graphs.
#'
#' The \code{print} method first calls the \code{to_SPSS} method followed
#' by \code{\link{to_latex}}.  Further customization can be done by calling
#' those two functions separately, and modifying the object returned by
#' \code{to_SPSS}.
#'
#' @param \dots  for \code{regression}, at least one formula specifying a
#' regression model.  Different models can be compared by supplying multiple
#' formulas.  For  the \code{to_SPSS} and \code{print} methods, additional
#' arguments to be passed down to \code{\link{format_SPSS}}.  For the
#' \code{plot} method, additional arguments to be passed down to
#' \code{\link{histSPSS}} or \code{\link{plotSPSS}}, in particular graphical
#' parameters.  For other methods, this is currently ignored.
#' @param data  a data frame containing the variables.
#' @param labels  a character or numeric vector giving labels for the
#' regression models in the output tables.
#' @param object,x  an object of class \code{"regression_SPSS"} as returned by
#' function \code{regression}.
#' @param statistics  a character string or vector specifying which SPSS tables
#' to produce.  Available options are \code{"summary"} for model summaries,
#' \code{"anova"} for ANOVA results, and \code{"estimates"} for estimated
#' coefficients.  For the \code{to_SPSS} method, only one option is allowed
#' (the default is the table of ANOVA results), but the \code{print} method
#' allows several options (the default is to print all tables).
#' @param change  a logical indicating whether tests on the
#' \eqn{R^2}{R-squared} change should be included in the table with model
#' summaries (if \code{statistics = "summary"}).  The default is \code{FALSE}.
#' @param version  a character string specifying whether the table or plot
#' should mimic the content and look of recent SPSS versions (\code{"modern"})
#' or older versions (<24; \code{"legacy"}).  For the table, the main
#' difference in terms of content is that small p-values are displayed
#' differently.
#' @param standardized  a logical indicating whether to return standardized
#' residuals and fitted values (\code{TRUE}), or residuals and fitted values on
#' their original scale (\code{FALSE}).
#' @param y  ignored (only included because it is defined for the generic
#' function \code{\link[graphics]{plot}}).
#' @param which  a character string specifying which plot to produce.  Possible
#' values are \code{"histogram"} for a histogram of the residuals, or
#' \code{"scatter"} for a scatterplot of the standardized residuals against the
#' standardized fitted values.
#'
#' @return  An object of class \code{"regression_SPSS"} with the following
#' components:
#' \describe{
#'   \item{\code{models}}{a list in which each component is an ojbect of class
#'   \code{"lm"} as returned by function \code{\link[stats]{lm}}.}
#'   \item{\code{summaries}}{a list in which each component is an ojbect of
#'   class \code{"summary.lm"} as returned by the
#'   \code{\link[stats:summary.lm]{summary}} method for objects of class
#'   \code{"lm"}.}
#'   \item{\code{response}}{a character string containing the name of the
#'   response variable.}
#'   \item{\code{method}}{a character string specifying whether the nested
#'   models are increasing in dimension by entering additional variables
#'   (\code{"enter"}) or decreasing in dimension by removing variables
#'   (\code{"remove"}).}
#' }
#'
#' The \code{to_SPSS} method returns an object of class \code{"SPSS_table"}
#' which contains all relevant information in the required format to produce
#' the LaTeX table.  See \code{\link{to_latex}} for possible components and
#' how to further customize the LaTeX table based on the returned object.
#'
#' The \code{print} method produces a LaTeX table that mimics the look of SPSS
#' output.
#'
#' The \code{coef}, \code{df.residual}, \code{fitted} and \code{residuals}
#' methods return the coefficients, residual degrees of freedom, fitted
#' values and residuals, respectively, of the \emph{last} model (to mimic
#' SPSS functionality).
#'
#' Similarly, the \code{plot} method returns the specified plot for the
#' \emph{last} model as an object of class \code{"\link[ggplot2]{ggplot}"},
#' which produces the plot when printed.
#'
#' @note
#' LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
#' may require several LaTeX compilations to be displayed correctly.
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
#'                    data = Eredivisie, labels = 2:4)
#' print(fit3, change  = TRUE)      # print LaTeX table
#' plot(fit3, which = "histogram")  # diagnostic plot
#'
#' @keywords multivariate
#'
#' @importFrom stats lm model.frame na.pass
#' @export

regression <- function(..., data, labels = NULL) {

  ## initializations
  formulas <- list(...)
  if (is.null(labels)) labels <- seq_along(formulas)
  names(formulas) <- labels
  n_formulas <- length(formulas)
  # check if response is the same in all models
  response <- vapply(formulas, function(x) as.character(x[[2]]), character(1))
  response <- unique(response)
  if (length(response) > 1) {
    stop("the same response must be used for all models")
  }
  # extract response and predictor matrix for each formula
  xy_list <- lapply(formulas, function(f) {
    mf <- model.frame(f, data = data, na.action = na.pass,
                      drop.unused.levels = TRUE)
    terms <- attr(mf, "terms")
    list(x = model.matrix(terms, data = mf), y = model.response(mf),
         intercept = attr(terms, "intercept"))
  })
  # check whether all formulas include an intercept
  have_intercept <- vapply(xy_list, function(xy) xy$intercept == 1, logical(1))
  if (!all(have_intercept)) stop("all models must have an intercept")
  # check whether models are nested and store index of largest model
  if (n_formulas == 1) {
    largest <- 1
    method <- "enter"
  } else {
    # check if we have a forward series (adding variables) or backward series
    # (removing variables) of nested models
    var_names <- lapply(xy_list, function(xy) colnames(xy$x))
    forwards <- backwards <- rep.int(NA, n_formulas - 1)
    for (i in seq(n_formulas - 1)) {
      forwards[i] <- (length(var_names[[i]]) < length(var_names[[i+1]])) &&
        all(var_names[[i]] %in% var_names[[i+1]])
      backwards[i] <- (length(var_names[[i+1]]) < length(var_names[[i]])) &&
        all(var_names[[i+1]] %in% var_names[[i]])
    }
    if (!all(forwards) && !all(backwards)) {
      stop("you must specify a series of nested models")
    }
    # store index of largest model
    if (all(forwards)) {
      largest <- n_formulas
      method <- "enter"
    } else {
      largest <- 1
      method = "remove"
    }
  }

  ## remove observations with missing values in the largest model
  yx <- cbind(xy_list[[largest]]$y, xy_list[[largest]]$x)
  names(yx)[1] <- response
  keep <- complete.cases(yx)

  ## fit linear models
  models <- lapply(formulas, lm, data = data[keep, ])
  summaries <- lapply(models, summary)

  ## return results
  out <- list(models = models, summaries = summaries,
              response = response, method = method)
  class(out) <- "regression_SPSS"
  out
}


#' @rdname regression
#' @importFrom stats aggregate anova model.matrix model.response pf
#' @export

to_SPSS.regression_SPSS <- function(object,
                                    statistics = c("estimates", "anova", "summary"),
                                    change = FALSE,
                                    version = r2spss_options$get("version"),
                                    ...) {

  ## initializations
  statistics <- match.arg(statistics)
  version <- match.arg(version, choices = get_version_values())
  legacy <- version == "legacy"
  models <- object$models
  k <- length(models)
  summaries <- object$summaries
  labels <- names(models)
  # define footnote that lists the response variable
  footnote_response <- paste("Dependent Variable:", object$response)
  # define footnotes that list predictors in regression models
  footnotes_predictors <- vapply(models, function(m) {
    strings <- c("Predictors: (Constant)", names(coef(m))[-1])
    paste(strings, collapse = ", ")
  }, character(1), USE.NAMES = FALSE)


  ## put requested results into SPSS format
  if (statistics == "summary") {

    ## initializations
    change <- isTRUE(change)
    wrap <- if(change) 90 else 50
    ## compute model fit statistics in SPSS format
    rsq <- vapply(summaries, "[[", numeric(1), "r.squared")
    adjrsq <- vapply(summaries, "[[", numeric(1), "adj.r.squared")
    sigma <- vapply(summaries, "[[", numeric(1), "sigma")
    fits <- data.frame("R" = sqrt(rsq), "R Square" = rsq,
                       "Adjusted R Square" = adjrsq,
                       "Std. Error of the Estimate" = sigma,
                       row.names = labels, check.names = FALSE)
    ## if requested, compute change statistics in SPSS format
    if (change) {
      # extract degrees of freedom of each model
      df <- vapply(summaries, function(s) as.integer(s$fstatistic[2:3]),
                   integer(2))
      # compute R square changes
      rsqchange <- diff(rsq)
      # compute full R square and degrees of freedom of the F test on the
      # change in R square
      if (object$method == "enter") {
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
      fchange <- c(summaries[[1]]$fstatistic[1],
                   (abs(rsqchange) / df1) / ((1 - rsqfull) / df2))
      df1 <- c(df[1, 1], df1)
      df2 <- c(df[2, 1], df2)
      p <- pf(fchange, df1, df2, lower.tail=FALSE)
      # construct data frame containing tests on change in R square
      changes <- data.frame("R Square Change" = c(rsq[1], rsqchange),
                            "F Change" = fchange, "df1" = df1, "df2" = df2,
                            "Sig. F Change" = p, row.names = labels,
                            check.names = FALSE)
      # add to information on model fits
      fits <- cbind(fits, changes)
    }
    ## format table nicely
    cn <- c("Model", names(fits))
    if (change && !legacy) {
      args <- list(fits, ...)
      if (is.null(args$p_value)) {
        args$p_value <- grepl("Sig.", names(fits), fixed = TRUE)
      }
      formatted <- do.call(format_SPSS, args)
    } else formatted <- format_SPSS(fits, ...)
    ## define header with line breaks
    cn <- c("Model", names(fits))
    limit <- rep_len(if (change) 10 else 15, length(cn))
    limit[grepl("Adjusted", cn, fixed = TRUE)] <- 9
    header <- wrap_text(cn, limit = limit)
    # add a top-level header if we have change statistics
    if (change) {
      # top-level header
      first <- which(cn == "R Square Change")
      last <- grep("Sig.", cn)
      standardized <- which(cn == "Beta")
      top <- data.frame(first = c(seq_len(first-1), first),
                        last = c(seq_len(first-1), last),
                        text = c(rep.int("", first-1), "Change Statistics"))
      # define header
      header <- list(top, header)
    }
    ## define footnotes
    row <- seq_len(k)
    column <- rep.int(1, k)
    footnotes <- wrap_text(footnotes_predictors, limit = wrap)
    footnotes <- data.frame(marker = letters[seq_len(k)], row = row,
                            column = column, text = footnotes)
    ## construct list containing all necessary information
    spss <- list(table = formatted, main = "Model Summary",
                 header = header, row_names = TRUE, info = 0,
                 footnotes = footnotes)
    if (change) spss$version <- version

  } else if (statistics == "anova") {

    # initializations
    wrap <- 66
    # compute ANOVA tables in SPSS format
    anovas <- mapply(function(m, s, l) {
      # perform ANOVA
      a <- anova(m)
      class(a) <- "data.frame"
      # aggregate information from individual variables
      a$Type <- ifelse(row.names(a) == "Residuals", "Residual", "Regression")
      sum <- aggregate(a[, c("Sum Sq", "Df")],
                       a[, "Type", drop = FALSE],
                       sum)
      mean <- aggregate(a[, "Mean Sq", drop = FALSE],
                        a[, "Type", drop = FALSE],
                        mean)
      a <- cbind(sum[, c("Sum Sq", "Df")], mean[, "Mean Sq", drop = FALSE])
      # extract information from test
      f <- s$fstatistic[1]
      p <- pf(f, s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
      # combine everything in a data frame and add total sum of squares
      data.frame("Model" = c(l, "", ""),
                 "Type" = c(sum$Type, "Total"),
                 "Sum of Squares" = c(a[, "Sum Sq"], sum(a[, "Sum Sq"])),
                 "df" = c(a[, "Df"], sum(a[, "Df"])),
                 "Mean Square" = c(a[, "Mean Sq"], NA_real_),
                 "F" = c(f, NA_real_, NA_real_),
                 "Sig." = c(p, NA_real_, NA_real_),
                 row.names = NULL, check.names = FALSE,
                 stringsAsFactors = FALSE)
    }, models, summaries, labels, SIMPLIFY=FALSE, USE.NAMES = FALSE)
    anovas <- do.call(rbind, anovas)
    # format table nicely
    if (legacy) formatted <- format_SPSS(anovas, ...)
    else {
      args <- list(anovas, ...)
      if (is.null(args$p_value)) args$p_value <- names(anovas) == "Sig."
      formatted <- do.call(format_SPSS, args)
    }
    # define header with line breaks
    cn <- gsub("Type", "", names(anovas), fixed = TRUE)
    header <- wrap_text(cn, limit = 12)
    # define footnotes
    row <- c("main", seq(from = 1, by = 3, length.out=k))
    column <- c(NA_integer_, rep.int(ncol(anovas), k))
    footnotes <- wrap_text(c(footnote_response, footnotes_predictors),
                           limit = wrap)
    footnotes <- data.frame(marker = letters[seq_len(k+1)], row = row,
                            column = column, text = footnotes)
    # construct list containing all necessary information
    spss <- list(table = formatted, main = "ANOVA", header = header,
                 row_names = FALSE, info = 2, footnotes = footnotes,
                 major = 3 * seq_len(k-1), version = version)

  } else if (statistics == "estimates") {

    ## compute coefficient tables in SPSS format
    coefficients <- mapply(function(m, s, l) {
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
      data.frame(Model = c(l, rep.int("", nrow(coef)-1)),
                 Variable = c("(Constant)", rownames(coef)[-1]),
                 coef[, c("B", "Std. Error")], Beta = beta,
                 coef[, c("t", "Sig.")], row.names = NULL,
                 check.names = FALSE, stringsAsFactors = FALSE)
    }, models, summaries, labels, SIMPLIFY=FALSE, USE.NAMES = FALSE)
    ## obtain number of rows in each coefficient table
    p <- vapply(coefficients, nrow, integer(1))
    ## combine coefficient tables
    coefficients <- do.call(rbind, coefficients)
    ## format table nicely
    if (legacy) formatted <- format_SPSS(coefficients, ...)
    else {
      args <- list(coefficients, ...)
      if (is.null(args$p_value)) args$p_value <- names(coefficients) == "Sig."
      formatted <- do.call(format_SPSS, args)
    }
    ## define header with line breaks
    cn <- gsub("Variable", "", names(coefficients), fixed = TRUE)
    # top-level header
    B <- which(cn == "B")
    SE <- which(cn == "Std. Error")
    standardized <- which(cn == "Beta")
    top <- data.frame(first = c(seq_len(B-1), B, standardized,
                                seq(from = standardized+1, length(cn))),
                      last = c(seq_len(B-1), SE, standardized,
                               seq(from = standardized+1, length(cn))),
                      text = c(rep.int("", B-1),
                               "Unstandardized\nCoefficients",
                               "Standardized\nCoefficients",
                               rep.int("", length(cn)-standardized)))
    # define header
    header <- list(top, cn)
    ## define footnotes
    footnotes <- data.frame(marker = "a", row = "main",
                            column = NA_integer_,
                            text = footnote_response)
    ## construct list containing all necessary information
    spss <- list(table = formatted, main = "Coefficients", header = header,
                 row_names = FALSE, info = 2, footnotes = footnotes,
                 major = cumsum(p[-k]), version = version)

  } else stop ("type of 'statistics' not supported")  # shouldn't happen

  # add class and return object
  class(spss) <- "SPSS_table"
  spss

}


#' @rdname regression
#' @export

print.regression_SPSS <- function(x,
                                  statistics = c("summary", "anova", "estimates"),
                                  change = FALSE,
                                  version = r2spss_options$get("version"),
                                  ...) {

  ## initializations
  count <- 0
  statistics <- match.arg(statistics, several.ok = TRUE)
  version <- match.arg(version, choices = get_version_values())

  ## print LaTeX table for descriptives
  if ("summary" %in% statistics) {
    cat("\n")
    # put frequencies into SPSS format
    spss <- to_SPSS(x, statistics = "summary", change = change,
                    version = version, ...)
    # print LaTeX table
    to_latex(spss, version = version)
    cat("\n")
    count <- count + 1
  }

  ## print LaTeX table for Levene test
  if ("anova" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put frequencies into SPSS format
    spss <- to_SPSS(x, statistics = "anova", version = version, ...)
    # print LaTeX table
    to_latex(spss, version = version)
    cat("\n")
  }

  ## print LaTeX table for ANOVA
  if ("estimates" %in% statistics) {
    if (count == 0) cat("\n")
    else cat("\\medskip\n")
    # put frequencies into SPSS format
    spss <- to_SPSS(x, statistics = "estimates", version = version, ...)
    # print LaTeX table
    to_latex(spss, version = version)
    cat("\n")
  }

}


#' @rdname regression
#' @importFrom stats coef
#' @export

coef.regression_SPSS <- function(object, ...) {
  nm <- length(object$models)
  coef(object$models[[nm]])
}


#' @rdname regression
#' @importFrom stats df.residual
#' @export

df.residual.regression_SPSS <- function(object, ...) {
  nm <- length(object$models)
  df.residual(object$models[[nm]])
}


#' @rdname regression
#' @importFrom stats fitted sd
#' @export

fitted.regression_SPSS <- function(object, standardized = FALSE, ...) {
  # extract fitted values from the last model
  nm <- length(object$models)
  fitted <- fitted(object$models[[nm]])
  # standardize if requested
  if (standardized) fitted <- (fitted - mean(fitted)) / sd(fitted)
  # return (standardized) fitted values
  fitted
}


#' @rdname regression
#' @importFrom stats df.residual residuals
#' @export

residuals.regression_SPSS <- function(object, standardized = FALSE, ...) {
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
#' @importFrom stats fitted residuals
#' @export

plot.regression_SPSS <- function(x, y, which = c("histogram", "scatter"),
                                 version = r2spss_options$get("version"),
                                 ...) {
  # initializations
  which <- match.arg(which)
  version <- match.arg(version, choices = get_version_values())
  # default title
  title <- paste0("Dependent Variable: ", x$response)
  # construct data frame containing relevant information
  data <- data.frame(fitted = fitted(x, standardized = TRUE),
                     residual = residuals(x, standardized = TRUE))
  # create requested plot
  if (which == "histogram") {
    ## histogram
    # default x-axis label
    xlab <- "Regression Standardized Residual"
    # define local function that overrides default for normal density
    local_histogram <- function(..., normal = TRUE) {
      histogram(..., normal = normal)
    }
    # create histogram of standardized residuals
    local_histogram(data, "residual", version = version, ...) +
      labs(title = title, x = xlab)
  } else if (which == "scatter") {
    ## scatter plot
    # default axis lables
    xlab <- "Regression Standardized Predicted Value"
    ylab <- "Regression Standardized Residual"
    # scatter plot of standardized residuals vs standardized fitted values
    scatter_plot(data, c("fitted", "residual"), version = version, ...) +
      labs(title = title, x = xlab, y = ylab)
  }
}
