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
	\item{header}{A data frame with plots data.}
	\item{species}{Species list as a \code{\linkS4class{taxlist}}
		object.}
	\item{relations}{A list including popup lists as data frames.}
	\item{coverconvert}{A scale conversion object of class
		\code{\linkS4class{coverconvert}}.}
}
}
\details{
This class was designed to include information of relevés, header data and
species in just one object. Objects can be created by calls of the
form \code{new("vegtable", ...)}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{tv2vegtable}}.
}
\examples{
library(vegtable)

showClass("vegtable")
}
