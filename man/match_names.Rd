\name{match_names}
\docType{methods}
\alias{match_names}
\alias{match_names,character,vegtable-method}

\title{Search matchings between character and taxlist objects.}
\description{
Names provided in a character vector will be compared with names stored in slot
\code{'taxonNames'} of an object of class \code{\linkS4class{taxlist}} by using
the function \code{\link[stringdist]{stringsim}}.
}
\usage{
\S4method{match_names}{character,vegtable}(x, object, ...)
}
\arguments{
\item{x}{A character vector with names to be compared.}
\item{object}{An object of class \code{\linkS4class{taxlist}} to be compared
	with.}
\item{...}{Further arguments passed to \code{\link[taxlist]{match_names}}.}
}
\details{
This method is applied to the slot \code{'species'} in the input
\code{\linkS4class{vegtable}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[taxlist]{match_names}}, \code{\link[stringdist]{stringsim}}.
}
\examples{
## No example at the moment
}
\keyword{methods}
