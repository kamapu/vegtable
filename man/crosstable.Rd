\name{crosstable}
\docType{methods}
\alias{crosstable}
\alias{crosstable,formula,data.frame-method}
\alias{crosstable,formula,vegtable-method}

\title{Generating Cross Tables from Database Lists.}
\description{
This function is generating cross tables, which are the most common format used
by further statistical packages (e.g. vegan).
}
\usage{
\S4method{crosstable}{formula,data.frame}(formula, data, FUN, na_to_zero=FALSE, ...)

\S4method{crosstable}{formula,vegtable}(formula, data, FUN, na_to_zero=FALSE, ...)
}
\arguments{
\item{formula}{A formula indicating the variables used in the cross table.}
\item{data}{Either a data frame or an object of class
	\code{\link[=vegtable-class]{vegtable}}.}
\item{FUN}{Funtion used to aggregate values.}
\item{na_to_zero}{A logical value indicating whether zeros should be inserted
	into empty cells or not.}
\item{...}{Further arguments passed to the function \code{\link{aggregate}}.}
}
\details{
The formula may have the form \code{'abundance ~ plot + species + \ldots'}.
Additional variables used for rows (\code{\ldots}) can be for instance the
layers.

For objects of class \code{\link[=vegtable-class]{vegtable}}, the formula can
also include variables from the species list (for example \code{AcceptedName}).
}
\value{
An object of class \code{\link{data.frame}}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
library(vegtables)
data(sweadataveg)

## Only first 20 observations
sweadataveg <- sweadataveg[1:20,]

## Cross table of species versus plots
crosstable(COVER_PERC ~ RELEVE_NR + AcceptedName, sweadataveg, max, na_to_zero=TRUE)

## In the case that author names and layers are desired
crosstable(COVER_PERC ~ RELEVE_NR + AcceptedName + AuthorName + LAYER,
	sweadataveg, max, na_to_zero=TRUE)

## Total cover of families per plot
crosstable(COVER_PERC ~ RELEVE_NR + FAMILY, sweadataveg, sum, na_to_zero=TRUE)
}
