\name{braun_blanquet-data}
\docType{data}
\alias{braun_blanquet}
\alias{braun_blanquet-data}

\title{Conversion of Braun-Blanquet codes to cover percentage.}
\description{
Data set as \code{\linkS4class{coverconvert}} object.
}
\usage{
data(braun_blanquet)
}
\format{
An object of class \code{\linkS4class{coverconvert}}.
}
\examples{
library(vegtable)
data(braun_blanquet)

## Add a custom scale
braun_blanquet$my_scale <- list(
	value=factor(c("low","medium","high"), levels=c("low","medium","high")),
	conversion=c(0,50,75,100))
summary(braun_blanquet)
braun_blanquet$my_scale
}
\keyword{datasets}
