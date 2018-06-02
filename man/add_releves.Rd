\name{add_releves}
\docType{methods}
\alias{add_releves}
\alias{add_releves,vegtable,data.frame-method}

\title{Merge relevés from data frames into vegtable objects.}
\description{
Addition of plot observations into existing data sets may implicate merging data
frames with \code{\linkS4class{vegtable}} objects.
}
\usage{
\S4method{add_releves}{vegtable,data.frame}(vegtable, releves, header,
	abundance, split_string, usage_ids=FALSE, layers=FALSE, layers_var,
	format="crosstable", ...)
}
\arguments{
\item{vegtable}{An object of class \code{\linkS4class{vegtable}}.}
\item{releves}{A data frame including plot observations to be added into
	\code{'vegtable'}.}
\item{header}{A data frame (optional) including header information for plots.}
\item{abundance}{A character value (or vector of length 2) indicating the names
	of abundance variable in \code{'vegtable'}.}
\item{split_string}{Character value used to split mixed abundance codes.}
\item{usage_ids}{Logical value indicating whether species are as taxon usage ids
	(integers) or names in \code{'releves'}.}
\item{layers}{Logical value indicating whether layers are included in
	\code{'releves'} or not.}
\item{layers_var}{Name of the layer variable in \code{'vegtable'}.}
\item{format}{Character value indicating input format of \code{'releves'}
	(whether \code{'crosstable'} or \code{'databaselist'}).}
\item{...}{Further arguments passed to function \code{\link{cross2db}}
	(i.e. \code{'na_strings'}).}
}
\details{
Since this function will only update slots \code{'samples'} and \code{'header'},
consistency with slots \code{'layers'}, \code{'relations'} and \code{'species'}
have to be checked and accordingly updated in advance.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{cross2db}}.
}
\examples{
## No example at the moment
}
\keyword{methods}
