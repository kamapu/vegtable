\name{df2vegtable}
\docType{methods}
\alias{df2vegtable}
\alias{df2vegtable,data.frame-method}

\title{Convert a data frame into a vegtable object.}
\description{
Conversion of a data frame containing a crosstable of abundance or cover of
species in single plots.
}
\usage{
df2vegtable(x, species, layer, ...)
}
\arguments{
\item{x}{A data frame formatted for a taxlist object.}
\item{species}{numeric or integer indicating the position of the column with
	species names.}
\item{layer}{numeric or integer indicating the position of the column with
	layers.}
\item{...}{further arguments passed from or to other methods.}
}
\details{
This function coerces a data frame containing a vegetation cross table into a
\code{\linkS4class{vegtable}}.
The input data frame \code{x} may include information on the layers or not.
}
\value{
A \code{\linkS4class{vegtable} object}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add an example
}
\keyword{methods}
