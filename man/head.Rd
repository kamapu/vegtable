\name{header}
\docType{methods}
\alias{header}
\alias{header,vegtable-method}
\alias{header<-}
\alias{header<-,vegtable,data.frame-method}
\alias{[,vegtable-method}
\alias{[,vegtable,ANY,ANY,ANY-method}
\alias{[<-}
\alias{[<-,vegtable,ANY,ANY,ANY-method}
\alias{$,vegtable-method}
\alias{$<-}
\alias{$<-,vegtable-method}

\title{Access to and Replacement of Slot header in vegtable Objects.}
\description{
Methods for quick access to slot \code{'header'} of
\code{\link[=vegtable-class]{vegtable}} objects.
Also replacement methods are implemented.
}
\usage{
\S4method{header}{vegtable}(x, ...)

\S4method{$}{vegtable}(x, name)

\S4method{[}{vegtable}(x, i, j, ..., drop=FALSE)

header(x) <- value
x$i <- value
x[i,j] <- value
}
\arguments{
\item{x}{Object of class \code{vegtable}.}
\item{value}{Data frame to be set as slot \code{'header'}.}
\item{...}{Further arguments passed to or from other methods.}
\item{name}{A name to access.}
\item{i,j}{Indices for access.}
\item{drop}{A logical value passed to \code{\link{Extract}}.}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add example for this function
}
\keyword{methods}
