\name{set_group}
\docType{methods}
\alias{set_group}
\alias{set_group,shaker,taxlist,character-method}
\alias{set_group,shaker,vegtable,character-method}
\alias{set_pseudo}
\alias{set_pseudo,shaker,taxlist,character-method}
\alias{set_pseudo,shaker,vegtable,character-method}
\alias{set_formula}
\alias{set_formula,shaker,taxlist,character-method}
\alias{set_formula,shaker,vegtable,character-method}

\title{Set slots in shaker objects.}
\description{
Properly set of slots in \code{\linkS4class{shaker}} objects.
}
\usage{
\S4method{set_pseudo}{shaker,taxlist,character}(shaker, companion, pseudo,
	pseudo_id, authority=FALSE, enc_cont="latin1", enc_gr="utf8", ...)

\S4method{set_pseudo}{shaker,vegtable,character}(shaker, companion, pseudo, ...)

\S4method{set_group}{shaker,taxlist,character}(shaker, companion, group, group_id,
	authority=FALSE, enc_cont="latin1", enc_gr="utf8", ...)

\S4method{set_group}{shaker,vegtable,character}(shaker, companion, group, ...)

\S4method{set_formula}{shaker,taxlist,character}(shaker, companion, formula,
	formula_id, authority=FALSE, enc_cont="latin1", enc_gr="utf8", ...)

\S4method{set_formula}{shaker,vegtable,character}(shaker, companion, formula, ...)
}
\arguments{
\item{shaker}{Describe.}
\item{companion}{Describe.}
\item{group,pseudo}{Describe.}
\item{formula}{Describe.}
\item{group_id,pseudo_id,formula_id}{Describe.}
\item{authority}{Describe.}
\item{enc_cont,enc_gr}{Describe.}
\item{...}{Describe.}
}
\details{}
\value{}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{}}
}
\examples{
## Add example for this function
}
