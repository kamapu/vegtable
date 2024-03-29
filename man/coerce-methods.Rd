% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coerce-methods.R
\name{as}
\alias{as}
\alias{as.list,vegtable-method}
\alias{coerce,vegtable,list-method}
\alias{coerce,coverconvert,list-method}
\alias{as.list,coverconvert-method}
\title{Coerce objects to lists}
\usage{
\S4method{as.list}{vegtable}(x, ...)

\S4method{as.list}{coverconvert}(x, ...)
}
\arguments{
\item{x}{An object to be coerced.}

\item{...}{further arguments passed from or to other methods.}
}
\value{
An object of class \code{list}.
}
\description{
Coerce vegtable objects to a list with every slot as a component of the list.
This way allows to explore content and solve problems when validity checks
fail.

Coercion is applied for different classes by vegtable.
}
\examples{
## vegtable as list
veg <- as(Kenya_veg, "list")
names(veg)

## coverconvert as list
as(Kenya_veg@coverconvert, "list")

}
\author{
Miguel Alvarez \email{kamapu78@gmail.com}
}
