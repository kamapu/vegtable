% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset.R
\name{subset}
\alias{subset}
\alias{subset,vegtable-method}
\title{Subset functions for vegtable objects}
\usage{
\S4method{subset}{vegtable}(
  x,
  subset,
  slot = "header",
  keep_children = FALSE,
  keep_parents = FALSE,
  relation,
  ...
)
}
\arguments{
\item{x}{A \linkS4class{vegtable} object for subset.}

\item{subset}{Logical expression for the subset.}

\item{slot}{Character value indicating the slot used as reference for subset.
At the moment only the values "taxonNames", "taxonRelations",
"taxonTraits", "header", "samples", and "relations" are accepted.
The three first values will be applied to the respective slots in the
contained \linkS4class{taxlist} object (slot \strong{species}).}

\item{keep_children}{Argument passed to \code{\link[taxlist:get_children]{taxlist::get_children()}}.}

\item{keep_parents}{Argument passed to \code{\link[taxlist:get_children]{taxlist::get_parents()}}.}

\item{relation}{Character value indicating the relation (slot \strong{relations})
to be used as reference for subset.}

\item{...}{Further arguments passed from or to other methods.}
}
\value{
A S4 object of class \linkS4class{vegtable}.
}
\description{
Produce subsets of \linkS4class{vegtable} objects.

Logical operations can be applied either to the plots, or the
relations, which are the main slots in that class.

This method can be referred to the slot \code{species} the same way as
\code{\link[taxlist:subset]{taxlist::subset()}}, then the rest of the data will include only
references to the subset of species list.
}
\examples{
## Subset by taxon name
Kenya_sub <- subset(
  x = Kenya_veg, subset = TaxonName == "Tagetes",
  slot = "taxonNames", keep_children = TRUE, keep_parents = TRUE
)
summary(Kenya_sub)
summary(Kenya_sub@species)

## Subset by taxon relations
Kenya_sub <- subset(
  x = Kenya_veg, subset = Level == "species",
  slot = "taxonRelations"
)
summary(Kenya_sub)
summary(Kenya_sub@species)

## Subset by taxon traits
Kenya_sub <- subset(
  x = Kenya_veg, subset = lf_behn_2018 == "obligate_annual",
  slot = "taxonTraits"
)
summary(Kenya_sub)
summary(Kenya_sub@species)

## Subset by header
Kenya_sub <- subset(x = Kenya_veg, subset = ALTITUDE <= 1000, slot = "header")
summary(Kenya_sub)

## Subset by samples (after converting coverage)
Kenya_veg <- cover_trans(x = Kenya_veg, to = "cover_percentage", rule = "middle")
Kenya_sub <- subset(x = Kenya_veg, subset = cover_percentage >= 50, slot = "samples")
summary(Kenya_sub)

## Subset by relations
Kenya_sub <- subset(
  x = Kenya_veg, subset = as.integer(YEAR) >= 2000,
  slot = "relations", relation = "REFERENCE"
)
summary(Kenya_sub)
}
\author{
Miguel Alvarez \email{kamapu78@gmail.com}
}
