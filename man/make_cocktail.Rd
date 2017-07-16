\name{make_cocktail}
\docType{methods}
\alias{make_cocktail}
\alias{make_cocktail,shaker,vegtable-method}

\title{Produce a Cocktail Classification}
\description{
Classification of \code{\linkS4class{vegtable}} objects according to
\bold{Cocktail} algorithms implemented in a \code{\linkS4class{shaker}} object.
}
\usage{
\S4method{make_cocktail}{shaker,vegtable}(shaker, vegtable, which, cover,
	syntax="Syntax", FUN=sum, ...)
}
\arguments{
\item{shaker}{An object of class \code{\linkS4class{shaker}} containing the
	respective cocktail definitions.}
\item{vegtable}{An object of class \code{\linkS4class{vegtable}} containing the
	vegetation observations to be classified.}
\item{which}{Integer or character indicating the definition to be applied for
	classification.}
\item{cover}{Name of the cover variable in \code{vegtable}.}
\item{syntax}{Character value indicating the name of the retrieved variable
	including the final classification of plots.}
\item{FUN}{Function used for merging multiple occurrence of species in a single
	plot.}
\item{...}{Further arguments passed from or to other methods.}
}
\details{
Output included single columns for each vegetation unit to be retrieved.

An extra column (named by the argument \code{syntax}) included all vegetation
units, indicating with \code{'+'} plots classified in more than one vegetation
unit.
}
\value{
A data frame corresponding to the slot \code{header} in \code{vegtable},
including the results of Cocktail classification for the respective plots.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\linkS4class{shaker}}, \code{\linkS4class{vegtable}}.
}
\examples{
## Add example for this function
}
