\name{subset}
\docType{methods}
\alias{subset}
\alias{subset,vegtable-method}

\title{Subset functions for vegtable objects.}
\description{
Produce subsets of \code{\linkS4class{vegtable}} objects.
}
\usage{
\S4method{subset}{vegtable}(x, subset, slot="header", ...)
}
\arguments{
\item{x}{A \code{\linkS4class{vegtable}} object for subset.}
\item{subset}{Logical vector or operation for subset.}
\item{slot}{Slot to be applied for subset.}
\item{...}{Further arguments passed from or to other methods.}
}
\details{
This function generate subsets of \code{\linkS4class{vegtable}}
objects through logical operations.
Such operations can be applied either to the plots, the species or the relations,
which are the main slots in that class.
}
\value{
A S4 object of class \code{\linkS4class{vegtable}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(dune_veg)
summary(dune_veg)

## Select plots used as pastures
Pastures <- subset(dune_veg, Use == "Pasture", slot="header")
summary(Pastures)
}
\keyword{methods}
