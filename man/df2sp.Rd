\name{df2sp}
\docType{method} % data or function?
\alias{df2sp}
\alias{df2sp,data.frame-method}
\alias{df2sp,vegtable-method}

\title{Quick Mapping of Plots}
\description{
This function is a wrapper of \code{\link{writeOGR}} producing either spatial
objects or KML-files.
}
\usage{
\S4method{df2sp}{data.frame}(x, lon="Longitude", lat="Latitude", file, ...)
\S4method{df2sp}{vegtable}(x, lon="LONGITUDE", lat="LATITUDE", ...)
}
\arguments{
\item{x}{Either a data frame or an object of class
	\code{\link[=vegtable-class]{vegtable}}.}
\item{lon,lat}{Strings indicating the names of columns with longitude and
	latitude information.}
\item{file}{Character value indicating the name of the output file.}
\item{...}{Further arguments passed to \code{\link{writeOGR}}.}
}
\details{
This function assumes values for coordinates in decimal degrees for longitude
and latitude, and the datum \dQuote{WGS84}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Add example for this function
}
