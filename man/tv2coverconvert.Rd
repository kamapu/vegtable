\name{tv2coverconvert}
\alias{tv2coverconvert}

\title{Importing conversion tables from Turboveg databases.}
\description{
This function reads the content of cover conversion tables stored in Turboveg
and attempts to reformat them in a more comprehensive structure.
}
\usage{
tv2coverconvert(file, as.is=TRUE)
}
\arguments{
\item{file}{A connection to a DBF file containing conversion table in Turboveg.}
\item{as.is}{A logical value passed to \code{\link{read.dbf}}}
}
\details{
This function is used internally by \code{\link{tv2vegtable}}.
Short and long names of each cover scale will be added as attribute.
}
\value{
A \code{\linkS4class{coverconvert}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{tv2vegtable}}, \code{\link[foreign]{read.dbf}}.
}
\examples{
library(vegtable)

## Installed Turboveg version of 'Fujiwara et al. (2014)'
TV_Home <- file.path(path.package("vegtable"), "tv_data", "popup", "Swea")
Table <- tv2coverconvert(file.path(TV_Home, "TVSCALE.DBF"))

## First scale have to be deleted from conversion table
Table@value <- Table@value[-1]
Table@conversion <- Table@conversion[-1]
summary(Table)
}
