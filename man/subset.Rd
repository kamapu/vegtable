\name{subset}
\docType{methods}
\alias{subset}
\alias{subset,vegtable-method}

\title{Subset Functions for 'vegtable' Objects.}
\description{
A subset of \code{\link[=vegtable-class]{vegtable}} objects will be done either
by plots, species or relations.
}
\usage{
subset(x, subset, slot="header", ...)
}
\arguments{
\item{x}{a \code{\linkS4class{vegtable}} object for subset.}
\item{subset}{logical vector or operation for subset.}
\item{slot}{slot to be applied for subset.}
\item{...}{further arguments passed from or to other methods.}
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
