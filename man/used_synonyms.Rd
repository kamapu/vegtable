% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/used_synonyms.R
\name{used_synonyms}
\alias{used_synonyms}
\alias{used_synonyms.vegtable}
\alias{used_synonyms,vegtable-method}
\alias{used_concepts}
\alias{used_concepts.vegtable}
\alias{used_concepts,vegtable-method}
\title{Retrieve synonyms or taxon concepts used in a data set}
\usage{
used_synonyms(x, ...)

\method{used_synonyms}{vegtable}(x, ...)

used_concepts(x, ...)

\method{used_concepts}{vegtable}(
  x,
  keep_children = FALSE,
  keep_parents = FALSE,
  keep_synonyms = TRUE,
  ...
)
}
\arguments{
\item{x}{A \linkS4class{vegtable} object.}

\item{...}{Further arguments to be passed from or to another methods.}

\item{keep_children}{A logical argument indicating whether children of
selected taxa should be included in the output or not.
This argument passed to \code{\link[=get_children]{get_children()}}.}

\item{keep_parents}{A logical value indicating whether parents of
selected taxa should be included in the output or not.
This argument passed to \code{\link[=get_parents]{get_parents()}}.}

\item{keep_synonyms}{A logical value indicating whether synonyms should be
included or not.}
}
\value{
The function \code{used_synonyms()} returns a data frame including following
variables:
\describe{
\item{SynonymID}{ID of the taxon usage name applied as synonym.}
\item{Synonym}{The synonym itself.}
\item{SynonymAuthor}{Author of synonym.}
\item{TaxonConceptID}{ID of the respective taxon concept.}
\item{AcceptedNameID}{ID of the taxon usage name set as accepted name of
the taxon concept.}
\item{AcceptedName}{The respective accepted name.}
\item{AcceptedNameAuthor}{The author of the accepted name.}
}

The function \code{used_concepts()} returns a \linkS4class{taxlist} object including
only taxa occurring in the plot observations of the input \linkS4class{vegtable}
object.
}
\description{
Plots records are rather linked to plant names than plant taxon concepts and
\code{used_synonyms()} lists all synonyms linked to records in a \linkS4class{vegtable}
object, including their respective accepted names.

On the other side, the function \code{used_concepts()} produces a subset of the
taxonomic list embeded in the slot \strong{species} including only taxonomic
concepts linked to records in the slot \strong{samples}.
}
\examples{
## Synonyms used in the Kenya_veg
Synonyms <- used_synonyms(Kenya_veg)
head(Synonyms)

## Subset species list to used concepts
species <- used_concepts(Kenya_veg)
Kenya_veg@species
species

}
\seealso{
\code{\link[=accepted_name]{accepted_name()}}
}
\author{
Miguel Alvarez \email{kamapu78@gmail.com}
}
