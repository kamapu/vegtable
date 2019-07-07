\name{names,dimnames}
\docType{methods}
\alias{names}
\alias{names,vegtable-method}
\alias{names,coverconvert-method}
\alias{dimnames}
\alias{dimnames,vegtable-method}
\alias{names<-}
\alias{names<-,vegtable-method}
\alias{names<-,coverconvert-method}

\title{Retrieve names of vegtable and coverconvert objects.}
\description{
Quick access to column names in slot header and names of conversion codes.
}
\usage{
\S4method{names}{coverconvert}(x)

\S4method{names}{vegtable}(x)

\S4method{dimnames}{vegtable}(x)
}
\arguments{
\item{x}{An object of class \code{\linkS4class{vegtable}} or
	\code{\linkS4class{coverconvert}}.}
}
\details{
These methods provide a quick display of the contents in
\code{\linkS4class{coverconvert}} and \code{\linkS4class{vegtable}} objects. 
}
\value{
Either a vector or a list (in the case of \code{'dimnames'}) with the names of
variables.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## All possibilities shown in 'Usage'
names(Kenya_veg@coverconvert)
names(Kenya_veg)
dimnames(Kenya_veg)
}
\keyword{methods}
