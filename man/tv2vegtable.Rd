\name{tv2vegtable}
\alias{tv2vegtable}

\title{Import vegetation data from a Turboveg databases.}
\description{
\code{tv2vegtable} imports vegetation data sets from Turboveg
data bases using the packages \code{foreign} and \code{\link{vegdata}}.
}
\usage{
tv2vegtable(db, tv_home=tv.home(), skip_empty_relations=TRUE, clean=TRUE)
}
\arguments{
\item{db}{name of Turboveg data base as character value.}
\item{tv_home}{Turboveg installation path as character value.}
\item{skip_empty_relations}{logical value indicating whether empty relations may
	be excluded from imported database or not.}
\item{clean}{logical value indicating whether output object should be cleaned or
	not.}
}
\details{
Import routine for Turboveg databases into an object of class
\code{\linkS4class{vegtable}}.

Most of the contents of Turboveg
databases are included in DBF-files and therefore imported by using the function
\code{\link{read.dbf}}.
Those elements
are basically database (list of samples and headerer data) and popup lists
(additional descriptions of factors included in header data as well as the cover
conversion table).
Empty columns in the header will be deleted in the imported
object.
Cover conversion table is split into single conversion tables by cover
scale, each as a two column \code{data.frame}.
The use of \code{\link{approxfun}} may be more appropriate for back conversion.

The respective species list will be imported by using the function
\code{\link[taxlist]{tv2taxlist}} from the package
\code{\link[=taxlist-package]{taxlist}}.
Additionally the imported species list will be restricted to 'observed taxa'
(only species occurring in plots).

In the cases where databases are organized by folders in Turboveg, the name of
the database should be indicated as \code{"folder\\db_name"}.
}
\value{
A S4 object of class \code{\linkS4class{vegtable}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[taxlist]{tv2taxlist}}, code{\link{tv2coverconvert}},
\code{\link[vegdata]{tv.home}}.
}

