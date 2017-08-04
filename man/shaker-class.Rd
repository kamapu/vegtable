\name{shaker-class}
\docType{class}
\alias{shaker}
\alias{shaker-class}

\title{Class containing Cocktail algorithms.}
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
\code{\linkS4class{vegtable}} object which is usually called \code{'companion'}.
Since modifications in the \code{companion} may affect the functionality of
the \code{'shaker'} object, it will be recommended to create the last during a
session by a source script instead recycling them from old R images.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{set_pseudo}}, \code{\link{set_group}}, \code{\link{set_formula}},
\code{\link{make_cocktail}}.
}
\examples{
library(vegtable)

showClass("shaker")
}
