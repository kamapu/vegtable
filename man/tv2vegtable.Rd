\name{tv2vegtable}
\alias{tv2vegtable}

\title{Import vegetation data from a \sQuote{Turboveg} databases.}
\description{
\code{tv2vegtable} imports vegetation data sets from \sQuote{Turboveg}
data bases.
}
\usage{
tv2vegtable(db, tv_home=tv.home(), skip_empty_relations=TRUE, skip_scale,
	clean=TRUE)
}
\arguments{
\item{db}{Name of \sQuote{Turboveg} data base as character value.}
\item{tv_home}{\sQuote{Turboveg} installation path as character value.}
\item{skip_empty_relations}{Logical value indicating whether empty relations may
	be excluded from imported database or not.}
\item{skip_scale}{Character value indicating scales to be excluded in slot
	\code{'coverconvert'}.}
\item{clean}{Logical value indicating whether output object should be cleaned or
	not.}
}
\details{
Import function for \sQuote{Turboveg} databases into an object of class
\code{\linkS4class{vegtable}}.
Most of the contents of \sQuote{Turboveg} databases are included in DBF files and
therefore imported by the function \code{\link[foreign]{read.dbf}}.
The automatic setting of database path will be done by the function
\code{\link[vegdata]{tv.home}} but it can be customised by the argument
\code{'tv_home'}.

The species list will be imported by using the function
\code{\link[taxlist]{tv2taxlist}} and therefore formatted as a
\code{\linkS4class{taxlist}} object.
Similarly, conversion tables will be handled as
\code{\linkS4class{coverconvert}} objects.

Empty columns in the header will be deleted in the imported object.
}
\value{
A \code{\linkS4class{vegtable}} object.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link[taxlist]{tv2taxlist}}, \code{\link{tv2coverconvert}},
\code{\link[vegdata]{tv.home}}.
}
\examples{
library(vegtable)

## Installed 'Turboveg' version of 'Fujiwara et al. (2014)'
TV_Home <- file.path(path.package("vegtable"), "tv_data")
Veg <- tv2vegtable("Fujiwara_2014", TV_Home)
summary(Veg)
}
