\name{Extract}
\docType{methods}
\alias{[,vegtable-method}
\alias{[,vegtable,ANY,ANY,ANY-method}
\alias{[<-}
\alias{[<-,vegtable,ANY,ANY,ANY-method}
\alias{$,vegtable-method}
\alias{$,coverconvert-method}
\alias{$<-}
\alias{$<-,vegtable-method}
\alias{$<-,vegtable,ANY-method}
\alias{$<-,coverconvert,list-method}

\title{Select or replace elements in a vegtable object.}
\description{
Methods for quick access to slot \code{'header'} of
\code{\linkS4class{vegtable}} objects.
Also replacement methods are implemented.
}
\usage{
\S4method{$}{vegtable}(x, name)

\S4method{[}{vegtable}(x, i, j, ..., drop=FALSE)

x$i <- value
x[i,j] <- value
}
\arguments{
\item{x}{Object of class \code{\linkS4class{vegtable}}.}
\item{value}{vector with values to be replaced in the respective position of
	slot \code{header}.}
\item{...}{Further arguments passed to or from other methods.}
\item{name}{A name to access.}
\item{i,j}{Indices for access.}
\item{drop}{A logical value passed to \code{\link{Extract}}.}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## Range of latitude values in database
range(Kenya_veg$LATITUDE)

## Summary of countries
summary(Kenya_veg$COUNTRY)
summary(droplevels(Kenya_veg$COUNTRY))

## First 5 samples
summary(Kenya_veg[1:5,])
}
\keyword{methods}
