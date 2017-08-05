\name{summary}
\docType{methods}
\alias{summary}
\alias{summary,vegtable-method}
\alias{summary,coverconvert-method}
\alias{summary,shaker-method}

\title{Summary method for vegtable objects.}
\description{
Display summaries for \code{\linkS4class{vegtable}} objects.
}
\usage{
\S4method{summary}{vegtable}(object, units="Kb", ...)

\S4method{summary}{coverconvert}(object, ...)

\S4method{summary}{shaker}(object, companion, authority=FALSE, ...)
}
\arguments{
\item{object}{Object to be summarized.}
\item{units}{Units used for object size (passed to \code{\link{format}}).}
\item{companion}{Companion object (either a \code{\linkS4class{taxlist}} or a
	\code{\linkS4class{vegtable}} object.}
\item{authority}{Logical value indicating whether authors should be displayed or
	not.}
\item{...}{further arguments to be passed to or from other methods.}
}
\details{
Those methods are implemented for objects of the classes
\code{\linkS4class{vegtable}}, \code{\linkS4class{coverconvert}} and
\code{\linkS4class{shaker}}.

The method for class \code{'vegtable'} retrieves the metadata, the size of the
object, its validity and additional statistics on the content of input object.

For objects of class \code{\linkS4class{shaker}}, the function \code{'summary'}
will either retrieve general statistics when \code{'companion'} is missing, or
a more detailed display when accompained by a \code{\linkS4class{taxlist}} or
\code{\linkS4class{vegtable}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)

## Summary for 'vegtable' objects
data(Wetlands)
summary(Wetlands_veg)

## Summary for 'coverconvert' objects
data(braun_blanquet)
summary(braun_blanquet)

## Summary for 'shaker' objects (alone and with companion)
summary(Wetlands)
summary(Wetlands, Wetlands_veg)
}
\keyword{methods}
