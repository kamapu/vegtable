\name{aggregate}
\docType{methods}
\alias{aggregate}
\alias{aggregate,formula-method}

\title{Aggregating information into a data frame.}
\description{
This function aggregates information contained in \code{\linkS4class{vegtable}}
objects to a summarizing data frame.
}
\usage{
\S4method{aggregate}{formula}(x, data, FUN, ...)
}
\arguments{
\item{x}{A formula indicating the variables used for the summary.}
\item{data}{Either a data frame or an object of class
	\code{\linkS4class{vegtable}}.}
\item{FUN}{Function used to aggregate values.}
\item{...}{Further arguments passed to the function
	\code{\link[stats]{aggregate}}.}
}
\details{
This function works in a similar way as \code{\link{crosstable}}.
}
\value{
An object of class \code{\link{data.frame}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[stats]{aggregate}}.
}
\examples{
## Example follows
}
\keyword{methods}
