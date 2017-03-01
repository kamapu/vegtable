\name{vegtable-class}
\docType{class}
\alias{vegtable-class}
\alias{vegtable}

\title{Class 'vegtable'.}
\description{
Class holding vegetation-plot data sets.
Designed to content all information stored in Turboveg-databases in just one
object.
}
\section{slots}{
\describe{
	\item{description}{A named character vector containing metadata.}
	\item{samples}{A data frame with samples list.}
	\item{head}{A data frame with plots data.}
	\item{species}{Species list as a \code{\link[=taxlist-class]{taxlist}}
		object.}
	\item{relations}{A list including popup lists as data frames.}
	\item{coverconvert}{A list with conversion tables for cover scales.}
	\item{syntax}{A list with action reports (from import and subset).}
}
}
\details{
This class was designed to include information of relevés, header data and
species in just one object. Objects can be created by calls of the
form \code{new("vegtable", ...)}.
Deletion done by
\code{\link{subset}} or "poolssp" will be recorded in the slot \code{log}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{import_vegtable}}.
}
\examples{
## Generating an empty 'vegtable' object
library(vegtables)
new("vegtable")
}
