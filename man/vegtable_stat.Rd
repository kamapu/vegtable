\name{vegtable_stat}
\alias{vegtable_stat}

\title{General statistics from vegtable objects.}

\description{
This function calculates general statistics of local \sQuote{Turboveg} databases
as required by GIVD (Global Index of Vegetation-Plot Databases,
\url{https://www.givd.info}).
}
\usage{
vegtable_stat(vegtable)
}
\arguments{
\item{vegtable}{An object of class \code{\linkS4class{vegtable}}.}
}
\details{
This function is based on a script delivered by GIVD for
summarising statistics required in the descriptions of databases (see meta data
in the page of the Global Index for Vegetation-Plot Databases).
}
\author{
GIVD. Adapted by Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## Statistics for GIVD
vegtable_stat(Kenya_veg)
}
