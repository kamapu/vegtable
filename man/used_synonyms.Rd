\name{used_synonyms}
\docType{methods}
\alias{used_synonyms}
\alias{used_synonyms,vegtable-method}

\title{Retrieve Synonyms used in the Data Set.}
\description{
Plots records are rather linked to plant names than plant taxon concepts.
This function provides a quick report about synonyms used in a data set (a
\code{\linkS4class{vegtable}} object) and their respective accepted name.
}
\usage{
\S4method{used_synonyms}{vegtable}(x, ...)
}
\arguments{
\item{x}{A \code{\linkS4class{vegtable}} object.}
\item{...}{Further arguments to be passed from or to another methods.}
}
\details{
This function will only retrieve synonyms that are used in plot records.
}
\value{
A data frame with following columns:
\describe{
	\item{SynonymsID}{Usage ID of synonyms.}
	\item{Synonym}{The synonym itself.}
	\item{SynonymAuthor}{Author of synonym.}
	\item{TaxonConceptID}{ID of the taxon concept.}
	\item{AcceptedNameID}{Usage ID of the respective accepted name.}
	\item{AcceptedName}{The respective accepted name.}
	\item{AcceptedNameAuthor}{The author of the accepted name.}
}
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{accepted_name}}.
}
\examples{
library(vegtable)
data(Kenya_veg)

## Synonyms used in the Kenya_veg
Synonyms <- used_synonyms(Kenya_veg)
head(Synonyms)
}
\keyword{methods}
