\name{count_taxa}
\docType{methods}
\alias{count_taxa}
\alias{count_taxa,vegtable-method}
\alias{count_taxa,formula-method}

\title{Count taxa within a taxlist object}
\description{
Counting number of taxa within \code{\linkS4class{taxlist}} objects or character
vectors containing taxon names.
}
\usage{
\S4method{count_taxa}{vegtable}(object, level, include_lower=FALSE, ...)

\S4method{count_taxa}{formula}(object, data, include_lower=FALSE, suffix="_count", in_header=FALSE,
	...)
}
\arguments{
\item{object}{An object of class \code{\linkS4class{vegtable}} or a formula.}
\item{data}{An object of class \code{\linkS4class{vegtable}}.}
\item{level}{Character value indicating the taxonomic rank of counted taxa.}
\item{include_lower}{Logical value, whether lower taxonomic ranks should be
	included at the requested level.}
\item{suffix}{Character value used as suffix on the calculated variable.}
\item{in_header}{Logical value, whether the result should be included in the
    slot header of the input \code{\linkS4class{vegtable}} object or not.
    A warning message is provided if the calculation is not done for every plot
    observation.}
\item{...}{further arguments passed among methods.}
}
\details{
This function provides a quick calculation of taxa in
\code{\linkS4class{vegtable}} objects, considering only records in slot samples.
Such records can be also merged from lower ranks.

For the formula method, units without any requested taxa will not appear in the
output data frame.
If no taxa at all is occurring at the requested level in any unit, an error
message will be retrieved.
}
\value{
An data frame with the number of taxa from requested level at requested units
for the formula method, or just an integer value.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(vegtable)

## Different alternatives
count_taxa(Kenya_veg)
head(count_taxa(~ ReleveID, Kenya_veg))
head(count_taxa(species ~ ReleveID, Kenya_veg))
head(count_taxa(species ~ ReleveID, Kenya_veg, TRUE))
head(count_taxa(family ~ ReleveID, Kenya_veg, TRUE))
}
\keyword{methods}
