\name{vegtable2kml}
\docType{methods}
\alias{vegtable2kml}
\alias{vegtable2kml,data.frame-method}
\alias{vegtable2kml,vegtable-method}

\title{Quick Mapping of Plot Observations.}
\description{
This function is a wrapper of \code{\link[plotKML]{kml}} producing and
displaying KML-files.
}
\usage{
\S4method{vegtable2kml}{data.frame}(obj, file, coords=~ Longitude + Latitude,
                srs=CRS("+proj=longlat +datum=WGS84"))
\S4method{vegtable2kml}{vegtable}(obj, file, coords=~ LONGITUDE + LATITUDE,
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
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## Plots containing Podocarpus observations
Kenya_veg@species <- subset(Kenya_veg@species, grepl("Podocarpus", TaxonName),
	slot="names")
	
Kenya_veg <- subset(Kenya_veg, TaxonUsageID \%in\%
	Kenya_veg@species@taxonNames$TaxonUsageID, slot="samples")

\dontrun{vegtable2kml(Kenya_veg, "Podocarpus.kml")}
}
\keyword{methods}
