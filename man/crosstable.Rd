\name{crosstable,cross2db}
\docType{methods}
\alias{crosstable}
\alias{crosstable,formula,data.frame-method}
\alias{crosstable,formula,vegtable-method}
\alias{cross2db}

\title{Generating cross tables from database lists.}
\description{
This function is generating cross tables, which are the most common format used
by statistical packages analysing vegetation data (e.g.
\code{\link[vegan]{vegan}}).
}
\usage{
\S4method{crosstable}{formula,data.frame}(formula, data, FUN, na_to_zero=FALSE, use_nas=TRUE, as_matrix=FALSE,
	...)

\S4method{crosstable}{formula,vegtable}(formula, data, FUN, na_to_zero=FALSE, use_nas=TRUE, ...)
	
cross2db(object, layers=FALSE, na_strings)
}
\arguments{
\item{formula}{A formula indicating the variables used in the cross table.}
\item{data}{Either a data frame or an object of class
	\code{\linkS4class{vegtable}}.}
\item{FUN}{Function used to aggregate values.}
\item{na_to_zero}{A logical value indicating whether zeros should be inserted
	into empty cells or not.}
\item{use_nas}{Logical value indicating whether NAs should be considered as
	levels for categorical variables or not.}
\item{as_matrix}{A logical value, whether output should be done as matrix or
	data frame.}
\item{...}{Further arguments passed to the function \code{\link{aggregate}}.}
\item{object}{A data frame including a cross table.}
\item{layers}{Logical value, whether the cross table includes a layer column or
	not.}
\item{na_strings}{Character vector indicating no records in the cross table.}
}
\details{
Most applications and displays of vegetation data use preferentially the cross
table format.
For convenience, the formula has the form
\code{'abundance ~ plot + species + \ldots'}.
Additional variables used for rows (\code{\ldots}) can be for instance the
layers.

For objects of class \code{\linkS4class{vegtable}}, the formula can
also include variables from the species list (for example \code{'AcceptedName'},
\code{'AuthorName'}) or even taxon traits.
}
\value{
An object of class \code{\link{data.frame}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
library(vegtable)

## load Kenya_veg and subset to reference 2331 (Bronner 1990)
data(Kenya_veg)
Kenya_veg <- subset(Kenya_veg, REFERENCE == 2331, slot="header")

## transform cover to percentage
Kenya_veg <- transform(Kenya_veg, to="cover_perc", rule="middle")

## cross table of the first 5 plots
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
        Kenya_veg[1:5,], mean, na_to_zero=TRUE)
head(Cross)
}
\keyword{methods}
