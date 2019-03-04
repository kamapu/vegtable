\name{veg_relation}
\docType{methods}
\alias{veg_relation}
\alias{veg_relation,vegtable,character-method}
\alias{veg_relation<-}
\alias{veg_relation<-,vegtable,character,data.frame-method}
\alias{relation2header}
\alias{relation2header,vegtable,data.frame-method}
\alias{relation2header,vegtable,character-method}

\title{Retrieve or replace relations in vegtable objects.}
\description{
Tables providing information about levels of categorical variables in the header
of a \sQuote{Turboveg} database are called \code{'popups'} in \sQuote{Turboveg},
but \code{'relations'} in \code{\link[vegtable]{vegtable}}.
Such variables will be converted into factors in the slot \code{'header'}
according to the levels and their sorting in the respective relation.
}
\usage{
\S4method{veg_relation}{vegtable,character}(vegtable, relation,
	match_header=FALSE, ...)

## Replacement method
veg_relation(vegtable, relation) <- value

\S4method{relation2header}{vegtable,data.frame}(vegtable, relation, by, vars, ...)

\S4method{relation2header}{vegtable,character}(vegtable, relation, ...)
}
\arguments{
\item{vegtable}{An object of class \code{\linkS4class{vegtable}}.}
\item{relation}{A character value indicating the relation table to be retrieved or
	replaced.}
\item{by}{Character value indicating the name of the common column used as
 	index for inserting values in slot 'header'.}
\item{vars}{A character vector with the names of variables to be inserted in
	slot header.}
\item{match_header}{A logical vector, whether only levels occurring in slot
	\code{'header'} should be considered or all.}
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
