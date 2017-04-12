\name{crosstable}
\docType{methods}
\alias{crosstable}
\alias{crosstable,formula,data.frame-method}
\alias{crosstable,formula,vegtable-method}

\title{Generating Cross Tables from Database Lists.}
\description{
This function is generating cross tables, which are the most common format used
by further statistical packages (e.g. vegan).
}
\usage{
\S4method{crosstable}{formula,data.frame}(formula, data, FUN, na_to_zero=FALSE, ...)

\S4method{crosstable}{formula,vegtable}(formula, data, FUN, paste_author=FALSE, na_to_zero=FALSE, ...)
}
\arguments{
\item{formula}{A formula indicating the variables used in the cross table.}
\item{data}{Either a data frame or an object of class
	\code{\linkS4class{vegtable}}.}
\item{FUN}{Funtion used to aggregate values.}
\item{na_to_zero}{A logical value indicating whether zeros should be inserted
	into empty cells or not.}
\item{paste_author}{should the author be pasted to the taxon name.}
\item{...}{Further arguments passed to the function \code{\link{aggregate}}.}
}
\details{
The formula may have the form \code{'abundance ~ plot + species + \ldots'}.
Additional variables used for rows (\code{\ldots}) can be for instance the
layers.

For objects of class \code{\link[=vegtable-class]{vegtable}}, the formula can
also include variables from the species list (for example \code{AcceptedName}).
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

## cross table of the first 5 releves
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
        Kenya_veg[1:5,], mean, na_to_zero=TRUE)
head(Cross)

## sum of species per plot
crosstable(AcceptedName ~ ReleveID, Kenya_veg[1:5,],
        function(x) length(unique(x)))
}
