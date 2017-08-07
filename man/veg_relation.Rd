\name{veg_relation}
\docType{methods}
\alias{veg_relation}
\alias{veg_relation,vegtable,character-method}
\alias{veg_relation<-}
\alias{veg_relation<-,vegtable,character,data.frame-method}

\title{Retrieve or replace relations in vegtable objects.}
\description{
Tables providing information about levels of categorical variables in the header
of a \sQuote{Turboveg} database are called \code{'popups'} in \sQuote{Turboveg},
but \code{'relations'} in \code{\link[vegtable]{vegtable}}.
Such variables will be converted into factors in the slot \code{'header'}
according to the levels and their sorting in the respective relation.
}
\usage{
\S4method{veg_relation}{vegtable,character}(vegtable, relation, ...)

veg_relation(vegtable, relation) <- value
}
\arguments{
\item{vegtable}{An object of class \code{\linkS4class{vegtable}}.}
\item{relation}{A character value indicating the veg_relation to be retrieved or
	replaced.}
\item{value}{A data frame containing the new veg_relation.}
\item{...}{Further arguments to be passed among methods.}
}
\value{
This function retrieves and object of class \code{'data.frame'}.
In the replacement method, an object of class
\code{\linkS4class{vegtable}} including \code{'value'} in the slot
\code{'relations'}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## overview of references
veg_relation(Kenya_veg, "REFERENCE")
}
\keyword{methods}
