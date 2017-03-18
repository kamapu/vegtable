\name{names}
\docType{methods}
\alias{names}
\alias{names,vegtable-method}
\alias{names,coverconvert-method}
\alias{dimnames}
\alias{dimnames,vegtable-method}
\alias{names<-}
\alias{names<-,vegtable-method}
\alias{names<-,coverconvert-method}

\title{Retrieve names of 'vegtable' and 'coverconvert' objects}
\description{
Quick access to column names in slot header and names of conversion codes.
}
\usage{
\S4method{names}{vegtable}(x)

\S4method{names}{coverconvert}(x)

\S4method{dimnames}{vegtable}(x)

names(x) <- value
}
\arguments{
\item{x}{an object of class \code{\linkS4class{vegtable}} or
	\code{\linkS4class{coverconvert}}.}
\item{value}{a character vector for replacing names.}
}
\details{
Some detailed explanations.
}
\value{
Output values
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add example for this function
}
\keyword{methods}
