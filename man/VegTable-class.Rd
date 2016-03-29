\name{VegTable-class}
\docType{class}
\alias{VegTable-class}
\alias{VegTable}
\title{Class 'VegTable'.}
\description{
Class holding vegetation-plot data sets. Initially designed to content all
information stored in Turboveg-databases in just one object.
}
\section{slots}{
\describe{
	\item{description}{A list describing the data set (metadata).}
	\item{samples}{A data frame with samples list.}
	\item{header}{A data frame with header data.}
	\item{species}{A data frame with species data.}
	\item{popups}{A list including popup lists, which may include additional
	information on factors contained in header data.}
	\item{coverconvert}{A list with conversion tables for cover scales.}
	\item{log}{A list with logs or action reports.}
}
}
\section{Methods}{
Methods defined with class 'VegTable' in the signature:
	\describe{
		\item{print}{\code{signature(x='VegTable')}: Print overview of
		'VegTable' objects.}
	}
	\describe{
		\item{subset}{\code{signature(x='VegTable')}: Subset data either by
		plots or by species.}
	}
}
\details{
This class was designed to include information of relevés, header data and
species in just one object. Objects can be created by calls of the
form \code{new('VegTable', ...)}. Deletion done by
\code{\link{subset}} or \code{\link{poolssp}} will be recorded
in the slot \code{log}.
}
\author{Miguel Alvarez.}
\examples{
## Generating an empty 'VegTable' object
library(vegtables)
new("VegTable")
}
