\name{set_pseudo,set_group,set_formula}
\docType{methods}
\alias{set_group}
\alias{set_group,shaker,taxlist,character-method}
\alias{set_group,shaker,vegtable,character-method}
\alias{set_pseudo}
\alias{set_pseudo,shaker,taxlist,character-method}
\alias{set_pseudo,shaker,vegtable,character-method}
\alias{set_formula}
\alias{set_formula,shaker,taxlist,character-method}
\alias{set_formula,shaker,vegtable,character-method}

\title{Set slots in shaker objects.}
\description{
Building \code{\linkS4class{shaker}} objects for Cocktail classifications.
}
\usage{
\S4method{set_pseudo}{shaker,taxlist,character}(shaker, companion, pseudo,
	pseudo_id, authority=FALSE, enc_cont="latin1", enc_gr="utf8", ...)

\S4method{set_pseudo}{shaker,vegtable,character}(shaker, companion, pseudo, ...)

\S4method{set_group}{shaker,taxlist,character}(shaker, companion, group,
	group_id, authority=FALSE, enc_cont="latin1", enc_gr="utf8", ...)

\S4method{set_group}{shaker,vegtable,character}(shaker, companion, group, ...)

\S4method{set_formula}{shaker,taxlist,character}(shaker, companion, formula,
	formula_id, authority=FALSE, enc_cont="latin1", enc_gr="utf8", ...)

\S4method{set_formula}{shaker,vegtable,character}(shaker, companion, formula,
	...)
}
\arguments{
\item{shaker}{Object of class \code{\linkS4class{shaker}} to be modified.}
\item{companion}{Either a \code{\linkS4class{taxlist}} or a
	\code{\linkS4class{vegtable}} object.}
\item{pseudo,group}{Character vector with names of taxa included in a
	pseudo-species or a species group.}
\item{formula}{Character vector including a formula as definition of a
	vegetation unit.}
\item{pseudo_id,group_id,formula_id}{Character value as name of the
	pseudo-species, species group or defined vegetation unit.}
\item{authority}{Logical value indicating whether author names should be
	included in the taxon name or not.}
\item{enc_cont,enc_gr}{Encodings used for special characters.}
\item{...}{Further arguments passes from or to other methods.}
}
\details{
These functions are implemented for constructing or complementing
\code{\linkS4class{shaker}} objects.
Note that construction of those objects will always require a \code{'companion'}
object, which is either an object of class \code{\linkS4class{taxlist}} or
\code{\linkS4class{vegtable}}.
}
\value{
A \code{\linkS4class{shaker}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\linkS4class{shaker}}, \code{\link{make_cocktail}}.
}
\examples{
library(vegtable)
data(Wetlands)

## Construct the 'shaker' object anew
Wetlands <- new("shaker")

## Set a pseudo-species
Wetlands <- set_pseudo(Wetlands, Wetlands_veg, c("Cyperus latifolius",
				"Cyperus exaltatus"))

## Set a species group
Wetlands <- set_group(Wetlands, Wetlands_veg, group_id="Cyperus papyrus",
		group=c(
                "Cyperus papyrus",
                "Cyclosorus interruptus",
                "Lepistemon owariense"))

## Set a fromula
Wetlands <- set_formula(Wetlands, Wetlands_veg, formula_id="HE1",
		formula="groups:'Cyperus papyrus' | species:'Cyperus papyrus > 50'")

## Summaries
summary(Wetlands)
summary(Wetlands, Wetlands_veg)
}
\keyword{methods}
