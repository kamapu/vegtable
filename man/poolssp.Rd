\name{poolssp}
\alias{poolssp}
\title{Pooling sub-specific Taxa into the Species Level.}
\description{
Sub-specific Taxa will be detected by counting words (names composed by more
than two words) and merged to the next species level when present.
}
\usage{
poolssp(vegtable)
}
\arguments{
	\item{vegtable}{an object of class
	\code{\link[=VegTable-class]{VegTable}}.}
}
\details{
This function check the length of \code{TaxonName} and reduces
longer names to the respective binary name (Genus species). This function may
therefore not work properly when \code{TaxonName} is also including authors
names.

This function is pooling taxa that belong to the same species, matching the two
first components of the name. In the slot \code{species} the merged
sub-specific syntaxa will be deleted and a logical column called
\code{Aggregated} will be inserted with the value \code{TRUE} for pooled
taxa.
}
\value{
A S4 object of class \code{\link[=VegTable-class]{VegTable}}.
}
\author{Miguel Alvarez.}
\seealso{
\code{\link{tv.site}}
}
\examples{
## Load data set
library(vegtables)
data(Africa_veg)
print(Africa_veg)

## Pool sub-species
Africa_veg <- poolssp(Africa_veg)
print(Africa_veg)

## Check which species were pooled
Africa_veg@species[Africa_veg@species$Aggregated,]
}

