\name{write_juice}
\docType{methods}
\alias{write_juice}
\alias{write_juice,vegtable,character,formula-method}

\title{Exporting Tables for \sQuote{Juice}.}
\description{
This function produce txt files as inport formats for \sQuote{Juice}
(\url{http://www.sci.muni.cz/botany/juice/}).
}
\usage{
\S4method{write_juice}{vegtable,character,formula}(data, file, db_name="Plot Observations",
	formula, FUN, header, coords, ...)
}
\arguments{
\item{data}{An object of class \code{\link[=vegtable-class]{vegtable}}.}
\item{file}{Character value indicating the name of output files (without file
	extension).}
\item{db_name}{Name for data set displayed in inport wizard.}
\item{formula}{A formula passed to \code{\link{crosstable}}.}
\item{FUN}{Funtion passed to \code{\link{crosstable}}.}
\item{header}{Variables of header to be exported.}
\item{coords}{Names of coordinate variables in header of \code{'data'}.}
\item{...}{Further arguments passed to the function \code{\link{crosstable}}.}
}
\details{
This function produces two output files to be imported into a \sQuote{Juice}
file:
A vegetation table produced by \code{\link{crosstable}} and a header table.
Both tables share the file name plus a suffix (\code{table} for the vegetation
table and \code{header} for the header).

For the import in \sQuote{Juice}, you may start with the table following in the
menu
\code{'File -> Import
-> Table -> from Spreadsheet File (e.g. EXCEL Table)'}
and then follow the wizard.
You further import the header table following in the menu \code{'File -> Import
-> Header Data
-> From Comma Delimited File'}.
Notice that the vegetation is a semi-colon delimited file, while the header is a
comma delimited file.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)
data(Kenya_veg)

## Only first 20 observations
Kenya_veg <- Kenya_veg[1:20,]
\dontrun{
write_juice(Kenya_veg, "SWEA", FUN=mean)
}
}
\keyword{methods}
