\name{print}
\docType{methods}
\alias{print}
\alias{print,VegTable-method}
\title{Print Function for 'VegTable' Objects.}
\description{
Overview of \code{\link[=VegTable-class]{VegTable}} objects will be displayed
in the R-console.
}
\usage{
\S4method{print}{VegTable}(x, ...)
}
\arguments{
	\item{x}{object of class \code{\link[=VegTable-class]{VegTable}}.}
	\item{...}{further arguments to be passed to or from other methods.}
}
\details{
This function return a quick overview about the amount of data included in the
respective \code{\link[=VegTable-class]{VegTable}} object. Such information
includes number of plots, number of species and number of variables.
Additionally an overview on the content of the slot \code{log} will be shown.
}
\author{Miguel Alvarez.}
\examples{
## Same information as in 'Africa_veg'
library(vegtables)
data(Africa_veg)
print(Africa_veg)
}
