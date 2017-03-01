\name{veg_relation}
\docType{methods}
\alias{veg_relation}
\alias{veg_relation,vegtable,character-method}
\alias{veg_relation<-}
\alias{veg_relation<-,vegtable,character,data.frame-method}

\title{Retrieve or replace relations in vegtable objects.}
\description{
Popup lists is a concept adopted from \bold{Turboveg} and they correspond to
tables providing information about categorical variables.
Such variables will be converted into factors in the slot \code{header} of
\code{'vegtable'} objects.
}
\usage{
\S4method{veg_relation}{vegtable,character}(vegtable, veg_relation, ...)
veg_relation(vegtable, veg_relation) <- value
}
\arguments{
\item{vegtable}{An object of class \code{\link[=vegtable-class]{vegtable}}.}
\item{veg_relation}{A character value indicating the veg_relation to be retrieved or
	replaced.}
\item{value}{A data frame containing the new veg_relation.}
\item{...}{Further arguments to be passed among methods.}
}
\value{
\code{veg_relation} retrieves and object of class \code{data.frame}.
In the replacement method, an object of class
\code{\linkS4class{vegtable}} including \code{value} in the slot
\code{relations}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add example for this function
}
