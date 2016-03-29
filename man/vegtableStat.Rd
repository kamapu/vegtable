\name{vegtableStat}

\alias{vegtableStat}

\title{General Statistics from 'VegTable' Data Sets.}

\description{
This function calculates general statistics of local TURBOVEG databases as
required by GIVD (Global Index of Vegetation-Plot Databases;
\url{http://botanik3.botanik.uni-greifswald.de/givd/}).
}
\usage{
vegtableStat(vegtable)
}
\arguments{
	\item{vegtable}{an object of class \code{\link[=VegTable-class]{VegTable}}.}
}
\details{
This function is based on a script delivered by GIVD for
summarising statistics required in the descriptions of databases (see metadata
in the page of the Global Index for Vegetation-Plot Databases).
}
\author{GIVD. Adapted by Miguel Alvarez.}
\examples{
## Same example as in 'Africa_veg' data
library(vegtables)
data(Africa_veg)
vegtableStat(Africa_veg)
}
