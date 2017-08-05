\name{dune_veg-data}
\docType{data}
\alias{dune_veg}
\alias{dune_veg-data}

\title{Dutch Dune Meadows.}
\description{
Data set from the package \code{\link[vegan]{vegan}}, converted to a
\code{\linkS4class{vegtable}} object.
}
\usage{
data(dune_veg)
}
\format{
An object of class \code{\linkS4class{vegtable}}.
}
\source{
Original data were imported from \code{\link[vegan]{dune}}.
}
\references{
\bold{Jongman RHG, ter Braak CJF, van Tongeren OFR (1987).} \emph{Data analysis
in community and landscape ecology.} Pudoc, Wageningen, NL.
}
\examples{
library(vegtable)
data(dune_veg)
summary(dune_veg)
}
\keyword{datasets}
