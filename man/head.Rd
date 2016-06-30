\name{head}
\docType{methods}
\alias{head}
\alias{head,vegtable-method}
\alias{head<-}
\alias{head<-,vegtable,data.frame-method}
\alias{[,vegtable-method}
\alias{[,vegtable,ANY,ANY,ANY-method}
\alias{[<-}
\alias{[<-,vegtable,ANY,ANY,ANY-method}
\alias{$,vegtable-method}
\alias{$<-}
\alias{$<-,vegtable-method}

\title{Access to and Replacement of Slot head in vegtable Objects.}
\description{
Methods for quick access to slot \code{'head'} of
\code{\link[=vegtable-class]{vegtable}} objects.
Also replacement methods are implemented.
}
\usage{
\S4method{head}{vegtable}(x, ...)

\S4method{$}{vegtable}(x, name)

\S4method{[}{vegtable}(x, i, j, ..., drop=FALSE)

head(x) <- value
x$i <- value
x[i,j] <- value
}
\arguments{
\item{x}{Object of class \code{vegtable}.}
\item{value}{Data frame to be set as slot \code{'head'}.}
\item{...}{Further arguments passed to or from other methods.}
\item{name}{A name to access.}
\item{i,j}{Indices for access.}
\item{drop}{A logical value passed to \code{\link{Extract}}.}
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Add example for this function
}
\keyword{methods}
