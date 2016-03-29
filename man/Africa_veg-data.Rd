\name{Africa_veg}
\docType{data}
\alias{Africa_veg}
\title{Vegetation plots from Kenya and Tanzania.}
\description{
Exemplary database containing vegetation-plots surveyed in the context of the
SWEA project (\url{http://www.ipe.uni-bonn.de/verbundprojekte/wetland}). The
data set is stored as a \code{\link[=VegTable-class]{VegTable}} object,
including 36 plots, 208 species records and 26 variables.
}
\usage{data(Africa_veg)}
\format{A data frame containing 30 variables for each observation plot.}
\source{Sakané et al. (2011).}
\references{
Sakané N, Alvarez M, Becker M, Böhme B, Handa C, Kamiri HW,
Langensiepen M, Menz G, Misana S, Mogha NG, Möseler BM, Mwita EJ, Oyieke HA,
van Wijk MT (2011). Classification, characterisation, and use of small
wetlands in East Africa. \emph{Wetlands} 31: 1103--1116.
}
\examples{
## Loading data sets
library(vegtables)
data(Africa_veg)

## Overview
print(Africa_veg)
}
\keyword{datasets}
