\name{merge_species}
\docType{methods}
\alias{merge_species}
\alias{merge_species,vegtable-method}

\title{Merge Sub-Specific Taxa into Corresponding Species in vegtable Objects}
\usage{
\S4method{merge_species}{vegtable}(taxlist, ...)
}
\arguments{
\item{taxlist}{Object of class \code{vegtable}.}
\item{...}{Further arguments passed to or from other methods.}
}
\description{
This function aggregate all sub-specific syntaxa in the respective species.
The aggregation occurs in the slot \code{species}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Add an example
}
