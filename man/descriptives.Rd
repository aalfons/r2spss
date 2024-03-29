% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptives.R
\name{descriptives}
\alias{descriptives}
\alias{to_SPSS.descriptives_SPSS}
\alias{print.descriptives_SPSS}
\title{Descriptive Statistics}
\usage{
descriptives(data, variables)

\method{to_SPSS}{descriptives_SPSS}(object, digits = 2, ...)

\method{print}{descriptives_SPSS}(x, version = r2spss_options$get("version"), ...)
}
\arguments{
\item{data}{a data frame containing the variables.}

\item{variables}{a character vector specifying numeric variables for which
to compute descriptive statistics.}

\item{object, x}{an object of class \code{"descriptives_SPSS"} as returned
by function \code{descriptives}.}

\item{digits}{an integer giving the number of digits after the comma to be
printed in the SPSS table.}

\item{\dots}{additional arguments to be passed down to
\code{\link{format_SPSS}}.}

\item{version}{a character string specifying whether the table should
mimic the look of recent SPSS versions (\code{"modern"}) or older versions
(<24; \code{"legacy"}).}
}
\value{
An object of class \code{"descriptives_SPSS"} with the following components:
\describe{
  \item{\code{classes}}{a character vector giving the (first) class of the
  variables of interest.}
  \item{\code{descriptives}}{a data frame containing the descriptive
  statistics.}
  \item{\code{n}}{an integer giving the number of observations.}
}

The \code{to_SPSS} method returns an object of class \code{"SPSS_table"}
which contains all relevant information in the required format to produce
the LaTeX table.  See \code{\link{to_latex}} for possible components and
how to further customize the LaTeX table based on the returned object.

The \code{print} method produces a LaTeX table that mimics the look of SPSS
output.
}
\description{
Compute descriptive statistics of numeric variables of a data set (number of
observations, minimum, maximum, mean, standard deviaiton).  The output is
printed as a LaTeX table that mimics the look of SPSS output.
}
\details{
The \code{print} method first calls the \code{to_SPSS} method followed
by \code{\link{to_latex}}.  Further customization can be done by calling
those two functions separately, and modifying the object returned by
\code{to_SPSS}.
}
\note{
LaTeX tables that mimic recent versions of SPSS (\code{version = "modern"})
may require several LaTeX compilations to be displayed correctly.
}
\examples{
# load data
data("Eredivisie")

# compute descriptive statistics for market value and age
descriptives(Eredivisie, c("MarketValue", "Age"))

}
\author{
Andreas Alfons
}
\keyword{univar}
