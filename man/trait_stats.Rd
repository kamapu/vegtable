\name{trait_stats}
\docType{methods}
\alias{trait_stats}
\alias{trait_stats,character,vegtable-method}
\alias{trait_stats,formula,vegtable-method}
\alias{trait_proportion}
\alias{trait_proportion,character,vegtable-method}
\alias{trait_proportion,formula,vegtable-method}

\title{Statistics for Taxon Traits}
\description{
Calculation of proportions of traits on plots or other groups, considering
either number of taxa or weighted values (by abundance, for instance).
}
\usage{
\S4method{trait_stats}{character,vegtable}(trait, object, FUN, head_var,
	taxon_level, merge_to, weight, suffix="_stats", in_header=FALSE, ...)

\S4method{trait_stats}{formula,vegtable}(trait, object, weight, suffix="_stats",
	in_header=FALSE, ...)

\S4method{trait_proportion}{character,vegtable}(trait, object, head_var,
	trait_level, taxon_level, merge_to, include_nas=TRUE, weight,
	suffix="_prop", in_header=FALSE, ...)

\S4method{trait_proportion}{formula,vegtable}(trait, object, in_header=FALSE,
	...)
}
\arguments{
\item{trait}{Either a character value or a formula.}
\item{object}{An object of class \code{\linkS4class{vegtable}}.}
\item{FUN}{A function usually defined as 'foo(x, ...)' or as 'foo(x, w, ...)'
	for weighted statistics.}
\item{head_var}{Variable from the slot 'header' used as unit for the calculation
	of proportions.}
\item{trait_level}{Character vector indicating a selection of levels from a
	trait, in the case that some levels should be ignored in the output.
	Only applied to the 'character' method.}
\item{taxon_level}{Character value indicating a selected taxonomic rank for the
	output.}
\item{merge_to}{Character value indicating the taxonomic rank for aggregation of
	taxa.}
\item{include_nas}{Logical value indicating whether NAs should be considered for
	the calculation of proportions or not.}
\item{weight}{Character value, the name of the variable in slot 'samples' used
	as weight for the proportions. Usually the numeric abundance.}
\item{suffix}{A suffix added to the levels of the trait at the output.}
\item{in_header}{Logical value indicating whether the output should be inserted
	in the slot 'header' of the input object or provided as data frame.}
\item{...}{Further arguments passed among methods. In the case of the 'formula'
	method, arguments are passed to the 'character' method.}
}
\details{
To be added.
}
\value{
A data frame with the proportions of traits levels or an object of class
\code{\linkS4class{vegtable}} including the proportions in the slot 'header'.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
## Coming soon.
}
\keyword{methods}
