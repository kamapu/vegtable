\name{shaker-class}
\docType{class}
\alias{shaker-class}
\alias{shaker}

\title{Class 'shaker'.}
\description{
Objects used for collecting Cocktail definitions.
}
\section{slots}{
\describe{
\item{pseudos}{list containing IDs of taxa that will be merged into
	pseudo-species.}
\item{groups}{list containing IDs of taxa belonging to the same Cocktail group.}
\item{dominants}{data frame including lists of species used as dominant species
	in Cocktail algorithms.}
\item{formulas}{list with formulas that will be used as definitions for
	vegetation units.}
}
}
\details{
A \code{shaker} object will be always dependent on a
\code{\linkS4class{vegtable}} object, thus the creation and manipulation of
these objects should be referred to the same version of the
\code{\linkS4class{vegtable}} one.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{make_cocktail}}.
}
\examples{
library(vegtable)

showClass("shaker")
}
