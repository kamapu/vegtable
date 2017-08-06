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
This function is used by \code{\link{tv2vegtable}} to import the respective
conversion table from \bold{Turboveg} databases.
Note that conversion tables in Turboveg have only stored the middle point for
each cover class in a scale, thus it will be recommended to rebuild the
\code{'coverconvert'} slot or use \code{\link{braun_blanquet}}.
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
Table <- tv2coverconvert(file.path(TV_Home, "tvscale.dbf"))

## First scale have to be deleted from conversion table
Table@value <- Table@value[-1]
Table@conversion <- Table@conversion[-1]
summary(Table)

## Compare the Turboveg version with a vegtable version
data(braun_blanquet)
summary(Table$br_bl)
summary(braun_blanquet$br_bl)
}
