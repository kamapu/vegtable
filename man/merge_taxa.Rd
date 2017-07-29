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
Details.
}
\value{
An object of class \code{\linkS4class{vegtable}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Insert example
}
\keyword{methods}
