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
\S4method{crosstable}{formula,data.frame}(formula, data, FUN,
	na_to_zero=FALSE, ...)
\S4method{crosstable}{formula,vegtable}(formula, data, FUN, na_to_zero=FALSE,
	...)
}
\arguments{
\item{formula}{A formula indicating the variables used in the cross table.}
\item{data}{Either a data frame or an object of class vegtable.}
\item{FUN}{Funtion used to aggregate values.}
\item{na_to_zero}{A logical value indicating whether zeros should be inserted
	into empty cells or not.}
\item{...}{Further arguments passed to the function \code{\link{aggregate}}.}
}
\details{
The formula may have the form 'abundance ~ plot + species + ...'.
The additional variables can be  for instance layers.
For objects of class vegtable, the formula also accesses to the content in the
species list, including AcceptedName.
}
\author{
Miguel Alvarez (\email{malvarez@uni-bonn.de}).
}
\examples{
## Add example for this function
}
