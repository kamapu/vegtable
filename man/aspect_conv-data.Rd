\name{aspect_conv-data}
\docType{data}
\alias{aspect_conv-data}
\alias{aspect_conv}

\title{Conversion of Aspect Classes to Azimuth}
\description{
Conversion table required to transform values of aspect to azimuth in degrees.
}
\usage{
data(aspect_conv)
}
\format{
A numeric vector of values in degrees for the symbols used as names.
}
\author{
Miguel Alvarez, \email{kamapu78@gmail.com}.
}
\examples{
library(vegtable)
data(aspect_conv)

aspect_conv[c("N", "S", "ENE", "SSW")]
}
\keyword{datasets}
