\name{Wetlands-data}
\docType{data}
\alias{Wetlands}
\alias{Wetlands-data}
\alias{Wetlands_veg}
\alias{Wetlands_veg-data}

\title{Vegetation-plots from Tanzania.}
\description{
A subset of \href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg} with plots
sampled in Tanzania.
}
\usage{
data(Wetlands)
}
\format{
An object of class \code{\linkS4class{shaker}} (\code{'Wetlands'}) and the
respective companion as \code{\linkS4class{vegtable}} object
(\code{'Wetlands_veg'}).
}
\source{
\url{http://www.givd.info/ID/AF-00-006}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}), Michael Curran
(\email{currmi01@gmail.com}).
}
\references{
\bold{Alvarez M (2017).} Classification of aquatic and semi-aquatic vegetation
in two East African sites: Cocktail definitions and syntaxonomy.
\emph{Phytocoenologia}.
}
\examples{
library(vegtable)
data(Wetlands)

summary(Wetlands)
summary(Wetlands_veg)
}
\keyword{datasets}
