\name{merge_taxa}
\docType{methods}
\alias{merge_taxa}
\alias{merge_taxa,vegtable,numeric-method}

\title{Merge concepts.}
\description{
Merge taxon concepts form into single ones.
}
\usage{
\S4method{merge_taxa}{vegtable,numeric}(object, concepts, ...)
}
\arguments{
\item{object}{Object of class \code{\linkS4class{vegtable}}.}
\item{concepts}{Numeric (integer) vector including taxon concepts to be merged.}
\item{...}{Further arguments passed to \code{\link[taxlist]{merge_taxa}}
	(\code{'taxlist'} method).}
}
\details{
This method is applied to a function defined in the package
\code{\link[=taxlist-package]{taxlist}} and only modify the slot
\code{'species'} in the input \code{'object'}.
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
}
\keyword{methods}
