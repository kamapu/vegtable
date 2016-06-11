\name{import_vegtable}
\alias{import_vegtable}

\title{Import Vegetation Data from Turboveg Databases.}
\description{
\code{import_vegtable} imports vegetation data sets from Turboveg
data bases using the packages \code{foreign} and \code{\link{vegdata}}.
}
\usage{
import_vegtable(db, tv_home=tv.home(), folder, skip_empty_popups=TRUE)
}
\arguments{
\item{db}{Name of Turboveg data base as character value.}
\item{tv_home}{Turboveg installation path as character value.}
\item{skip_empty_popups}{Logical value indicating wether empty popups may be
	excluded from imported database or not.}
}
\details{
Import routine for Turboveg databases into an object of class
\code{\link[=vegtable-class]{vegtable}}.

Most of the contents of Turboveg
databases are included in DBF-files and therefore imported by using the function
\code{\link{read.dbf}}.
Those elements
are basically database (list of samples and header data) and popup lists
(additional descriptions of factors included in head data as well as the cover
conversion table).
Empty columns in the head will be deleted in the imported
object. Cover conversion table is split into single conversion tables by cover
scale, each as a two column \code{data.frame}.
The use of \code{\link{approxfun}} may be more appropriate for back conversion.

The respective species list will be imported by using the function
\code{\link{tvsplist}} from the package \code{\link[=taxlist-package]{taxlist}}.
Additionally the imported species list will be restricted to 'observed taxa'
(only species occurring in plots).

In the cases where databases are organized by folders in Turboveg, the name of
the database should be indicated as \code{"folder\\db_name"}.
}
\value{
A S4 object of class \code{\link[=vegtable-class]{vegtable}}.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\seealso{
\code{\link{tvsplist}}, code{\link{import_coverconvert}}, \code{\link{tv.home}}.
}
\examples{
\dontrun{
## Example using data 'elbaue' from package vegdata
library(vegtables)
Elbaue <- import_vegtable("elbaue", file.path(path.package("vegtables"),
	"tvdata"))
print(Elbaue)
}
}
