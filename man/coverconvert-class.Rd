\name{coverconvert-class}
\docType{class}
\alias{coverconvert}
\alias{coverconvert-class}

\title{Cover conversion tables.}
\description{
Cover conversion tables for \code{\linkS4class{vegtable}} objects.
}
\section{Slots}{
\describe{
\item{value}{List containing the levels of each scale.}
\item{conversion}{List with the respective start and end cut levels for the
	scale levels.}
}
}
\details{
This class implements conversions from different cover scales in percentage
cover.
For transformations to percentage cover, the function \code{\link{transform}}
should be than used.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{tv2coverconvert}}, \code{\link{braun_blanquet}}.
}
\examples{
library(vegtable)
showClass("coverconvert")

## Add a custom scale
Scale <- new("coverconvert")
Scale$my_scale <- list(
	value=factor(c("low","medium","high"), levels=c("low","medium","high")),
	conversion=c(0,50,75,100))
summary(Scale)
}
\keyword{classes}
