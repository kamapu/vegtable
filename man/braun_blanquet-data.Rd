\name{braun_blanquet-data}
\docType{data}
\alias{braun_blanquet}
\alias{braun_blanquet-data}

\title{Conversion of Braun-Blanquet codes to cover percentage.}
\description{
Cover values conversion as \code{\linkS4class{coverconvert}} object.
}
\usage{
data(braun_blanquet)
}
\details{
Object of class \code{\linkS4class{coverconvert}} contains conversion tables
usually from a categorical variable (a cover scale) to a numerical one
(equivalent percentage cover value).
cover values are stored as range for each level in the scale (minimum and
maximum cover value).
}
\format{
An object of class \code{\linkS4class{coverconvert}}.
}
\seealso{
\code{\linkS4class{coverconvert}}, \code{\link{transform}}.
}
\examples{
library(vegtable)
data(braun_blanquet)

## Quick displays
names(braun_blanquet)
summary(braun_blanquet)
summary(braun_blanquet$b_bbds)
}
\keyword{datasets}
