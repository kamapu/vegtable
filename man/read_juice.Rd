\name{read_juice}
\alias{read_juice}

\title{Importing Juice Tables.}
\description{
This function imports vegetation tables exported from Juice
(\url{http://www.sci.muni.cz/botany/juice}).
}
\usage{
read_juice(file, encoding="LATIN-1", sep=";", na="", ...)
}
\arguments{
\item{file}{Character value indicating the name of the file exported from
	Juice.}
\item{encoding}{Argument passed to \code{\link{readLines}}.}
\item{sep}{Separator used to split rows into columns.}
\item{na}{Character used as missing value.}
\item{...}{Further arguments passed to \code{\link{readLines}}.}
}
\details{
For a properly import, you may strictly follow the export steps in Juice:
\itemize{
	\item Menu \code{File -> Export -> Table -> to Spreadsheet Format File}
	\item Check the option \code{Export covers in \%}
}

In the \code{header} (see \bold{value}), the first column (\code{juice_nr})
corresponds to the plot number assigned by \bold{Juice} at import, while the
column \code{db_nr} is the number originally assigned to the plot (e.g.
\bold{Turboveg} ID).
}
\value{
A list with two elements:
\describe{
	\item{cross_table}{A data frame of species by plot.}
	\item{header}{A data frame with header data.} 
}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add example for this function
## For the example a installed file will be required
}
