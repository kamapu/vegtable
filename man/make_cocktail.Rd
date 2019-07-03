\name{make_cocktail}
\docType{methods}
\alias{make_cocktail}
\alias{make_cocktail,shaker,vegtable-method}

\title{Produce a Cocktail classification.}
\description{
Classification of \code{\linkS4class{vegtable}} objects according to
\bold{Cocktail} algorithms.
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
Cocktail algorithms are logical functions selecting plots according to either
occurrence of species groups and cover values of single species.
A group will be declared as occurring in a plot when at least a half of its
members is present in the plot.

This function inserts single columns with logical values indicating whether a
plot is classified in the vegetation unit or not.
An additional column (name provided in argument \code{'syntax'}) compile all
vegetation units,
indicating with a \code{'+'} symbol those plots classified in more than one
vegetation unit.
When only a part of the formulas will be used, it should be specified by the
argument \code{'which'}.
}
\value{
A data frame corresponding to the slot \code{'header'} of input object
\code{'vegtable'},
including the results of Cocktail classification for the respective plots.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\references{
\bold{Alvarez M (2017).} Classification of aquatic and semi-aquatic vegetation
in two East African sites: Cocktail definitions and syntaxonomy.
\emph{Phytocoenologia}.

\bold{Bruelheide H (2000).} A new measure of fidelity and its application to
defining species groups. \emph{Journal of Vegetation Science} 11: 167--178.

\bold{Kočí M, Chytrý M, Tichý L (2003).} Formalized reproduction of an
expert-based phytosociological classification: a case study of subalpine
tall-forb vegetation. \emph{Journal of Vegetation Science} 14: 601--610.
}
\seealso{
\code{\linkS4class{shaker}}, \code{\linkS4class{vegtable}},
\code{\link{Wetlands}}.
}
\examples{
library(vegtable)

## Example from Alvarez (2017)
data(Wetlands)

Wetlands_veg@header <- make_cocktail(Wetlands, Wetlands_veg, cover="percen")
summary(as.factor(Wetlands_veg@header$Syntax))

## Same but only for two vegetation units
data(Wetlands)
Wetlands_veg@header <- make_cocktail(Wetlands, Wetlands_veg,
	which=c("HY1","HY2"), cover="percen")
summary(as.factor(Wetlands_veg@header$Syntax))
}
