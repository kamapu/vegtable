\name{header}
\docType{methods}
\alias{header}
\alias{header,vegtable-method}
\alias{header<-}
\alias{header<-,vegtable,data.frame-method}

\title{Retrieve or replace slot header in 'vegtable' objects.}
\description{
Retrieve or replace the content of slot \code{'header'} in
\code{\linkS4class{vegtable}} objects.
}
\usage{
\S4method{header}{vegtable}(x, ...)

header(x) <- value
}
\arguments{
\item{x}{Object of class \code{\linkS4class{vegtable}}.}
\item{value}{Data frame to be set as slot \code{header}.}
\item{...}{Further arguments passed to or from other methods.}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

head(header(Kenya_veg))
}
\keyword{methods}
