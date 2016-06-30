\name{obj2kml}
\docType{methods}
\alias{obj2kml}
\alias{obj2kml,data.frame-method}
\alias{obj2kml,vegtable-method}

\title{Quick Mapping of Plot Observations.}
\description{
This function is a wrapper of \code{\link[plotKML]{kml}} producing and
displaying KML-files.
}
\usage{
\S4method{obj2kml}{data.frame}(obj, file, coords=~ Longitude + Latitude,
                srs=CRS("+proj=longlat +datum=WGS84"))
\S4method{obj2kml}{vegtable}(obj, file, coords=~ LONGITUDE + LATITUDE,
                srs=CRS("+proj=longlat +datum=WGS84"))
}
\arguments{
\item{obj}{Input object containing coordinate values.}
\item{file}{Character value with the name of output file (including file
	extension).}
\item{coords}{Either a character vector or a formula indicating the names of
	coordinate values.}
\item{srs}{Spatial reference system as \code{proj4string}.}
}
\details{
Precise this function.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Add example for this function
}
