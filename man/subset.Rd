\name{subset}
\docType{methods}
\alias{subset}
\alias{subset_by_species}

\title{Subset Functions for 'vegtable' Objects.}
\description{
A subset of \code{\link[=VegTable-class]{VegTable}} objects will be done either
by plots, species or popups.
}
\usage{
subset_by_species(vegtable, ...)
# subset_by_head
# subset_by_popup
# subset_by_syntax
}
\arguments{
	\item{vegtable}{Object of class \code{\link[=vegtable-class]{vegtable}}.}
	\item{...}{Further arguments to be passed to or from other methods (see
		details).}
}
\details{
This function generate subsets of \code{\link[=vegtable-class]{vegtable}}
objects through logical operations.
Such operations can be applied either to the plots, the species or the popups,
which are the main slots in that class.

In \code{subset_by_species}, arguments in \code{...} are passed to
\code{\link[=subset,taxlist-method]{subset}} method for objects of class
\code{\link[=taxlist-class]{taxlist}}.
Superfluous information will be deleted from the slots \code{head} and
\code{popus} as well.
}
\value{A S4 object of class \code{\link[=VegTable-class]{VegTable}}.}
\author{Miguel Alvarez.}
\seealso{\code{\link{substitute}}}
\examples{
## Examples are comming soon
}
