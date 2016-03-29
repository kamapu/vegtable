\name{importVegTable}
\alias{importVegTable}
\title{Import Vegetation Data from Turboveg Databases.}
\description{\code{importVegTable} imports vegetation data sets from Turboveg
data bases using the packages \code{foreign} and \code{\link{vegdata}}.}
\usage{
importVegTable(db, tv_home, skip_empty_popups=TRUE)
}
\arguments{
	\item{db}{Name of Turboveg data base.}
	\item{tv_home}{Turboveg installation path.}
	\item{skip_empty_popups}{May empty popups be excluded from imported
	database?}
}
\details{
Import routine for Turboveg databases into an object of class
\code{\link[=VegTable-class]{VegTable}}.

Most of the contents of Turboveg
databases are included in DBF-files and therefore imported by using the function
\code{\link{read.dbf}}. Those elements
are basically database (list of samples and header data) and popup lists
(additional descriptions of factors included in head data as well as the cover
conversion table). Empty columns in the head will be deleted in the imported
object. Cover conversion table is split into single conversion tables by cover
scale, each as a two column \code{data.frame}. The use of
\code{\link{approxfun}} may be more appropriate for back conversion.

The respective species list will be imported by using the function
\code{\link{tax}} from the package \code{\link{vegdata}}. In such case, only
valid names will be included in the object.
}
\value{
A S4 object of class \code{\link[=VegTable-class]{VegTable}}.
}
\author{Miguel Alvarez.}
\seealso{
\code{\link{tax}} \code{\link{approxfun}}
}
\examples{
## Example using data 'elbaue' from package vegdata
library(vegtables)
Elbaue <- importVegTable("elbaue", file.path(path.package("vegtables"),
	"tvdata"))
print(Elbaue)
}
