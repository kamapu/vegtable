\name{subset}
\docType{methods}
\alias{subset}
\alias{subset_by_species}
\alias{subset_by_header}
\alias{subset_by_popup}

\title{Subset Functions for 'vegtable' Objects.}
\description{
A subset of \code{\link[=vegtable-class]{vegtable}} objects will be done either
by plots, species or relations.
}
\usage{
subset_by_species(vegtable, ...)
subset_by_header(vegtable, ...)
subset_by_popup(vegtable, popup, ...)
# subset_by_syntax
}
\arguments{
\item{vegtable}{Object of class \code{\link[=vegtable-class]{vegtable}}.}
\item{popup}{A character vector indicating the popup list used for subset.}
\item{...}{Further arguments to be passed to or from other methods (see
	details).}
}
\details{
This function generate subsets of \code{\link[=vegtable-class]{vegtable}}
objects through logical operations.
Such operations can be applied either to the plots, the species or the relations,
which are the main slots in that class.

In \code{subset_by_species}, arguments in \code{...} are passed to
\code{\link[=subset,taxlist-method]{subset}} method for objects of class
\code{\link[=taxlist-class]{taxlist}}.
Superfluous information will be deleted from the slots \code{header} and
\code{popus} as well.
}
\value{
A S4 object of class \code{\link[=vegtable-class]{vegtable}}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{\code{\link{substitute}}}
\examples{
## Examples are comming soon
}
