\name{transform}
\docType{methods}
\alias{transform}
\alias{transform,character,coverconvert-method}
\alias{transform,factor,coverconvert-method}
\alias{transform,vegtable,missing-method}

\title{Convert cover scales to percent cover.}
\description{
Convert values of a categorical cover scale to percentage values.
}
\usage{
\S4method{transform}{character,coverconvert}(x, conversion, from=NULL,
	rule="top", zeroto=0.1, ...)

\S4method{transform}{factor,coverconvert}(x, conversion, ...)

\S4method{transform}{vegtable,missing}(x, to, replace=FALSE, rule="top",
	zeroto=0.1, ...)
}
\arguments{
\item{x}{Either a factor or character vector, or a \code{\linkS4class{vegtable}}
	object.}
\item{conversion}{An object of class \code{\linkS4class{coverconvert}}.}
\item{from}{Scale name of values in \code{'x'} as character value.}
\item{to}{Name of the column in slot \code{'samples'} for writing converted
	values.}
\item{replace}{Logical value indicating whether existing cover values should be
	replaced or not.}
\item{rule}{Rule applied for the conversion (see details).}
\item{zeroto}{Value used to replace levels with bottom at 0\% cover.}
\item{...}{Further arguments passed from or to other methods.}
}
\details{
This function requires as input a \code{\linkS4class{coverconvert}} object which
contains the conversion tables.

In the case of \code{\linkS4class{vegtable}} objects, the conversion is already
embedded in the slot \code{'coverconvert'}.

Three rules are implemented for transformation, either \code{'top'} (values
transformed to the top of the range), \code{'middle'} (transformation at the
midpoint), and \code{'bottom'} (conversion at the lowest value of the range).
In the later case, transformation ranges starting at 0\% of cover can be set to
a different value by the argument \code{'zeroto'}.

When \code{'replace=FALSE'}, existing values of cover in the
\code{\linkS4class{vegtable}} object will be maintained.
Since there is not a standard naming of cover values, in the transformation the
name of cover variable should be indicated in the argument \code{'to'}.
}
\value{
Either a vector or a \code{\linkS4class{vegtable}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## Check the available scales
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
