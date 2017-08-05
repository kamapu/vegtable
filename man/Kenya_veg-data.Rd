\name{Kenya_veg-data}
\docType{data}
\alias{Kenya_veg-data}
\alias{Kenya_veg}

\title{Vegetation-Plots from Kenya.}
\description{
A subset of \href{http://www.givd.info/ID/AF-00-006}{SWEA-Dataveg} including
five references providing plots collected in Kenya.
}
\usage{
data(Kenya_veg)
}
\format{
An object of class \code{\linkS4class{vegtable}}.
}
\source{
\url{http://www.givd.info/ID/AF-00-006}.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}), Michael Curran
(\email{currmi01@gmail.com}).
}
\references{
\bold{Bronner G (1990).} \emph{Vegetation and land use in the Mathews Range area,
Samburu-District, Kenya}. J. Cramer, Berlin.

\bold{Bussmann RW (1994).} \emph{The forests of Mount Kenya -- vegetation,
ecology, destruction and management of a tropical mountain forest ecosystem}.
Universität Bayreuth. 

\bold{Bussmann RW (2002).} Islands in the desert -- forest vegetation of Kenya's
smaller mountains and highland areas. \emph{Journal of East African Natural
History} 91: 27--79.

\bold{Fujiwara K, Furukawa T, Kiboi SK, Mathenge S, Mutiso P, Hayashi H,
Meguro S (2014).} Forest types and biodiversity around the Great Rift Valley in
Kenya. \emph{Contributii Botanice} 49: 143--178.

\bold{Schmitt K (1991).} \emph{The vegetation of the Aberdare National Park
Kenya}. Universitätsverlag Wagner, Innsbruck.
}
\examples{
library(vegtable)
data(Kenya_veg)
summary(Kenya_veg)
}
\keyword{datasets}
