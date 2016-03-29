\name{crosstable}
\docType{methods}
\alias{crosstable}
\alias{crosstable,VegTable-method}
\title{Make vegetation cross tables.}
\description{Create a vegetation crosstable from a database list format.}
\usage{
crosstable(data, species, layers, plots, cover, progress="windows", ...)

\S4method{crosstable}{VegTable}(data, species, layers, plots, cover,
progress="windows", ...)
}
\arguments{
	\item{data}{column oriented data frame containing the information to build
	the cross table. Objects of class \code{\link[=VegTable-class]{VegTable}}
	are also accepted.}
	\item{species}{character indicating name of species column.}
	\item{layers}{character indicating name of layer column (optional).}
	\item{plots}{character indicating name of plots column.}
	\item{cover}{character indicating name of cover or abundance information,
	which may be a numeric vector.}
	\item{progress}{character indicating a kind of progress bar displayed (only
	"windows" and "text" are accepted as values).}
	\item{...}{further arguments to be passed to or from other methods.}
}
\details{
This function creates a cross table for vegetation data stored as database list.

Be careful with duplicated observations (more than one record for the same
species in a plot). In such a case I recommend the use of the function
\code{\link{aggregate}} previously.

Since this function is relatively slow working with big data sets, a progress
bar is implemented by using functions \code{\link{winProgressBar}} (default) or
\code{\link{txtProgressBar}} (optional).

If objects of class \code{\link[=VegTable-class]{VegTable}} are used, the cross
table will be created using the information contained in slot \code{samples}
only.
}
\value{A data frame with the first column indicating species and optionally a
second column indicating layers (if specified in arguments). The rest of the
columns contain the abundance of species in single plots (absence as NAs).}
\author{Miguel Alvarez.}
