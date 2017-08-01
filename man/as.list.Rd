\name{as.list}
\docType{methods}
\alias{as.list}
\alias{as.list,coverconvert-method}
\alias{as.list,vegtable-method}

\title{Coerce an S4 object to a list.}
\description{
Coercion used to explore content in S4 objects.
}
\usage{
\S4method{as.list}{vegtable}(x, ...)

\S4method{as.list}{coverconvert}(x, ...)
}
\arguments{
\item{x}{an object of class \code{\linkS4class{coverconvert}} or
	\code{\linkS4class{vegtable}}}.
\item{...}{further arguments passed from or to other methods.}
}
\details{
S4 objects will be coerced to lists, where each slot in the input object becomes
a member of the output list.
This way allows to explore content and solve problems when validity checks fail. 
}
\value{
An object of class \code{list}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Importing 'Easplist' from taxlist
library(taxlist)
data(Easplist)

## Head of slot 'taxonNames'
class(Easplist)
head(Easplist@taxonNames)

## The same after coercing to list
Easplist <- as.list(Easplist)
class(Easplist)
head(Easplist$taxonNames)
}
\keyword{methods}
