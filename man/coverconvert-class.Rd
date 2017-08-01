\name{coverconvert-class}
\docType{class}
\alias{coverconvert}
\alias{coverconvert-class}

\title{Cover conversion tables.}
\description{
Cover conversion tables for 'vegtable' objects.
}
\section{slots}{
\describe{
\item{value}{list containing the levels of each scale.}
\item{conversion}{list with the respective start, end and cut levels for the
	respective scale classes.}
}
}
\details{
Conversion tables are 
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\seealso{
\code{\link{tv2coverconvert}}.
}
\examples{
library(vegtable)
showClass("coverconvert")
}
\keyword{classes}
