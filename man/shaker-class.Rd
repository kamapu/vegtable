\name{shaker-class}
\docType{class}
\alias{shaker}
\alias{shaker-class}

\title{Class containing Cocktail algorithms.}
\description{
Objects used for collecting Cocktail definitions.
}
\section{Slots}{
\describe{
\item{pseudos}{List containing IDs of taxa that will be merged into
	pseudo-species.}
\item{groups}{List containing IDs of taxa belonging to the same Cocktail group.}
\item{dominants}{data frame including lists of species used as dominant species
	in Cocktail algorithms, as well as operators and cover values used in the
	formulas.}
\item{formulas}{List with formulas that will be used as definitions for
	vegetation units.}
}
}
\details{
These objects work as \bold{expert systems} for recognition of defined
vegetation units among plots of a \code{\linkS4class{vegtable}} object.
A \code{'shaker'} object will be always dependent on a
\code{\linkS4class{vegtable}} object, which is called \code{'companion'}.
Since modifications in the \code{companion} may affect the functionality of
the \code{'shaker'} object, it will be recommended to create the last during a
session by a source script instead of recycling them from old R images.
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
