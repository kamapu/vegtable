% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary}
\alias{summary}
\alias{summary,vegtable-method}
\alias{summary,coverconvert-method}
\alias{summary,shaker-method}
\alias{show,vegtable-method}
\alias{print,vegtable-method}
\alias{show,coverconvert-method}
\alias{print,coverconvert-method}
\alias{show,shaker-method}
\alias{print,shaker-method}
\title{Summary method for vegtable objects}
\usage{
\S4method{summary}{vegtable}(object, units = "Kb", ...)

\S4method{summary}{coverconvert}(object, ...)

\S4method{summary}{shaker}(object, companion, authority = FALSE, ...)

\S4method{show}{vegtable}(object)

\S4method{print}{vegtable}(x, ...)

\S4method{show}{coverconvert}(object)

\S4method{print}{coverconvert}(x, ...)

\S4method{show}{shaker}(object)

\S4method{print}{shaker}(x, ...)
}
\arguments{
\item{object, x}{Object to be summarized.}

\item{units}{Units used for object size (passed to \code{\link[=format]{format()}}).}

\item{...}{further arguments to be passed to or from other methods.}

\item{companion}{Companion object (either a \linkS4class{taxlist} or a
\linkS4class{vegtable} object.}

\item{authority}{Logical value indicating whether authors should be
displayed or not.}
}
\description{
Display summaries for \linkS4class{vegtable} objects.

Those methods are implemented for objects of the classes \linkS4class{vegtable},
\linkS4class{coverconvert} and \linkS4class{shaker}.

The method for class \code{vegtable} retrieves the metadata, the size of
the object, its validity and additional statistics on the content of input
object.

For objects of class \linkS4class{shaker}, the function \code{summary()} will either
retrieve general statistics when \code{companion} is missing, or a more detailed
display when accompained by a \linkS4class{taxlist} or \linkS4class{vegtable} object.
}
\examples{
## Summary for 'vegtable' objects
summary(Wetlands_veg)
## Summary for 'coverconvert' objects
summary(braun_blanquet)
## Summary for 'shaker' objects (alone and with companion)
summary(Wetlands, Wetlands_veg)
}
\author{
Miguel Alvarez \email{kamapu78@gmail.com}
}
