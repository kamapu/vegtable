\name{merge_taxa,taxa2samples}
\docType{methods}
\alias{merge_taxa}
\alias{merge_taxa,vegtable,numeric,missing-method}
\alias{merge_taxa,vegtable,missing,character-method}
\alias{taxa2samples}
\alias{taxa2samples,vegtable-method}

\title{Merge concepts.}
\description{
Merge taxon concepts form into single ones or insert accepted names to slot
samples.
}
\usage{
\S4method{merge_taxa}{vegtable,numeric,missing}(object, concepts, level, ...)

\S4method{merge_taxa}{vegtable,missing,character}(object, concepts, level, ...)

\S4method{taxa2samples}{vegtable}(object, merge_to, add_traits, ...)
}
\arguments{
\item{object}{Object of class \code{\linkS4class{vegtable}}.}
\item{concepts}{Numeric (integer) vector including taxon concepts to be merged.}
\item{level,merge_to}{Character value indicating the level to which the taxa
	have to be	merged.}
\item{add_traits}{A character vector indicating variables in the slot
	'taxonTraits' to be added in slot 'samples'.}
\item{...}{Further arguments passed to \code{\link[taxlist]{merge_taxa}}
	(\code{'taxlist'} method).}
}
\details{
This method is applied to a function defined in the package
\code{\link[=taxlist-package]{taxlist}} and only modify the slot
\code{'species'} in the input \code{'object'}.

The use of \code{'taxa2samples'} with \code{'merge_to'} argument will produce a
similar result as using \code{'merge_taxa'} with \code{'level'} argument, but
\code{'taxa2samples'} will replace the records in slot samples by the respective
accepted names without any modification in slot species.
Additionally taxon concept IDs will be addes as columns in samples and taxon
traits if indicated in argument \code{add_traits}.
}
\value{
An object of class \code{\linkS4class{vegtable}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## Merge Olea capensis into one
summary(subset(Kenya_veg@species, grepl("Olea capensis", TaxonName),
	slot="names"), "all")
Kenya_veg <- merge_taxa(Kenya_veg, c(52041,50432,50235))

## Check Olea capensis again
summary(subset(Kenya_veg@species, grepl("Olea capensis", TaxonName),
	slot="names"), "all")

## Effect of taxa2samples by counting taxa
count_taxa(Kenya_veg, level="genus")

Kenya_veg <- taxa2samples(Kenya_veg, merge_to="genus")
count_taxa(Kenya_veg, level="genus")
}
\keyword{methods}
