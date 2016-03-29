\name{subset}
\docType{methods}
\alias{subset}
\alias{subset,VegTable-method}
\title{Subset method for 'VegTable' Objects.}
\description{
A subset of \code{\link[=VegTable-class]{VegTable}} objects will be done either
by plots, species or popups.
}
\usage{
\S4method{subset}{VegTable}(x, plots, species, popups, ...)
}
\arguments{
	\item{x}{object of class \code{\link[=VegTable-class]{VegTable}}.}
	\item{plots}{logical vector or operation to be applied at the heads slot.}
	\item{species}{logical vector or operation to be applied at the species slot.}
	\item{popups}{list with logical vectors or operations to be applied at the
	popups slot.}
	\item{...}{further arguments to be passed to or from other methods.}
}
\details{
This function generate subsets of \code{\link[=VegTable-class]{VegTable}}
objects through logical operations. Such operations can be applied either to
the plots, the species or the popups, which are the main slots in that class.
Simultaneous operations to more than one of those elements are also allowed.

Logical operators provided to use the slot \code{popups} as subset criterion may
be provided in the form \code{list(popup list=substitute(logical operation))}
(for details, see \code{\link{substitute}}).
}
\value{A S4 object of class \code{\link[=VegTable-class]{VegTable}}.}
\author{Miguel Alvarez.}
\seealso{\code{\link{substitute}}}
\examples{
## Example of subset for 'VegTable' objects
library(vegtables)
data(Africa_veg)

## Subset by single elements
Africa_new <- subset(Africa_veg, plots=ALTITUDE < 1000)
print(Africa_new) # samples from lower altitudes

Africa_new <- subset(Africa_veg, species=LETTERCODE != "TYPHDOM")
print(Africa_new) # excluding Typha domingensis from data set

Africa_new <- subset(Africa_veg,
		popups=list(countries=substitute(COUNTRY_NAME == "Tanzania")))
print(Africa_new) # samples collected in Tanzania

## All criteria at once
Africa_new <- subset(Africa_veg, plots=ALTITUDE < 1000,
		species=LETTERCODE != "TYPHDOM",
		popups=list(countries=substitute(COUNTRY_NAME == "Tanzania")))
print(Africa_new)
}
