\name{as.list}
\docType{methods}
\alias{as.list}
\alias{as.list,coverconvert-method}
\alias{as.list,vegtable-method}

\title{Coerce an S4 object to a list.}
\description{
Coercion used to explore inconsistencies in S4 objects.
}
\usage{
\S4method{as.list}{vegtable}(x, ...)

\S4method{as.list}{coverconvert}(x, ...)
}
\arguments{
\item{x}{an object of class \code{\linkS4class{taxlist}},
	\code{\linkS4class{coverconvert}} or \code{\linkS4class{vegtable}}}
\item{...}{further arguments passed to or from other methods.}
}
\details{
Details.
}
\value{
An object of class \code{list}, whose elements correspond to slots from input
object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add example for this function
}
