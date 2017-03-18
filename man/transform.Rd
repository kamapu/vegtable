\name{transform}
\docType{methods}
\alias{transform}
\alias{transform,character,coverconvert-method}

\title{Convert cover scales to percent cover.}
\description{
Description.
}
\usage{
\S4method{transform}{character,coverconvert}(x, conversion, from=NULL, rule="top", zeroto=0.1, ...)
}
\arguments{
\item{x}{either character or vegtable object.}
\item{conversion}{object of class coverconvert.}
\item{from}{scale name of the x values.}
\item{rule}{rule applied for the conversion (see details).}
\item{zeroto}{value used to replace levels with bottom at 0\% cover.}
\item{...}{further arguments passed from or to other methods.}
}
\details{
More details
}
\value{
A vector?
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## Add example for this function
}
