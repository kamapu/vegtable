\name{transform}
\docType{methods}
\alias{transform}
\alias{transform,character,coverconvert-method}
\alias{transform,factor,coverconvert-method}
\alias{transform,vegtable,missing-method}

\title{Convert cover scales to percent cover.}
\description{
Description.
}
\usage{
\S4method{transform}{character,coverconvert}(x, conversion, from=NULL,
	rule="top", zeroto=0.1, ...)

\S4method{transform}{factor,coverconvert}(x, conversion, ...)

\S4method{transform}{vegtable,missing}(x, to, replace=FALSE, rule="top",
	zeroto=0.1, ...)
}
\arguments{
\item{x}{either character or vegtable object.}
\item{conversion}{object of class coverconvert.}
\item{from}{scale name of the x values.}
\item{to}{name of the column in slot 'samples' to store converted values.}
\item{replace}{logical value indicating wether existing cover values should be
	replaced or not.}
\item{rule}{rule applied for the conversion (see details).}
\item{zeroto}{value used to replace levels with bottom at 0\% cover.}
\item{...}{further arguments passed from or to other methods.}
}
\details{
More details
}
\value{
A vector?
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## Check the classes in the header
summary(Kenya_veg@coverconvert)

## Conversion by default 'top' rule
Kenya_veg <- transform(Kenya_veg, to="percent")
summary(as.factor(Kenya_veg@samples$percent))

## Conversion by 'middle' rule
Kenya_veg <- transform(Kenya_veg, to="percent", rule="middle", replace=TRUE)
summary(as.factor(Kenya_veg@samples$percent))

## Conversion by 'bottom' rule
Kenya_veg <- transform(Kenya_veg, to="percent", rule="bottom", replace=TRUE)
summary(as.factor(Kenya_veg@samples$percent))
}
\keyword{methods}
