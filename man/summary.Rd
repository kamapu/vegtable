\name{summary}
\docType{methods}
\alias{summary}
\alias{summary,vegtable-method}

\title{Summary Method for 'vegtable' Objects.}
\description{
Overview of \code{\linkS4class{vegtable}} objects will be displayed
in the R-console.
}
\usage{
\S4method{summary}{vegtable}(object, units="Kb", ...)
}
\arguments{
\item{object}{object of class \code{\link[=vegtable-class]{vegtable}}.}
\item{units}{character value indicating the units shown in the object's
	allocated space.}
\item{...}{further arguments to be passed to or from other methods.}
}
\details{
This function return a quick overview about the amount of data included in the
respective \code{\linkS4class{vegtable}} object.
Such information includes number of plots, number of species and number of
variables.
Additionally an overview on the content of the slot \code{log} will be shown.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)
summary(Kenya_veg)
}
