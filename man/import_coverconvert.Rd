\name{import_coverconvert}
\alias{import_coverconvert}

\title{Importing conversion tables from Turboveg databases.}
\description{
This function reads the content of cover conversion tables stored in Turboveg
and attempts to reformat them in a more comprehensive structure.
}
\usage{
import_coverconvert(file, as.is=TRUE)
}
\arguments{
\item{file}{A connection to a dbf-file containing conversion table in Turboveg.}
\item{as.is}{A logical value passed to \code{\link{read.dbf}}}
}
\details{
This function is used internally by \code{\link{tv2vegtable}}.
Short and long names of each cover scale will be added as attribute.
}
\value{
A list, where each element is a data frame including the classes of the
cover scale and the respective percentage cover value (mid-point) as columns.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{import_vegtable}}, \code{\link{read.dbf}}.
}
\examples{
## Add example for this function
}
