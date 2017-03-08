\name{dune_veg-data}
\docType{data}
\alias{dune_veg-data}
\alias{dune_veg}

\title{Dutch Dune Meadows.}
\description{
Data set from the package \code{vegan}, converted to a
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
\author{
Oksanen et al.
}
\examples{
library(vegtable)
data(dune_veg)
summary(dune_veg)
}
\keyword{datasets}
