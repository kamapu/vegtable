\name{popup}
\docType{methods}
\alias{popup}
\alias{popup,vegtable,character-method}
\alias{popup<-}
\alias{popup<-,vegtable,character,data.frame-method}

\title{Retrieve or Replace Popup Lists}
\description{
Popup lists is a concept adopted from \bold{Turboveg} and they correspond to
tables providing information about categorical variables.
Such variables will be converted into factors in the slot \code{head} of
\code{'vegtable'} objects.
}
\usage{
\S4method{popup}{vegtable,character}(vegtable, popup, ...)
popup(vegtable, popup) <- value
}
\arguments{
\item{vegtable}{An object of class \code{\link[=vegtable-class]{vegtable}}.}
\item{popup}{A character value indicating the popup to be retrieved or
	replaced.}
\item{value}{A data frame containing the new popup.}
\item{...}{Further arguments to be passed among methods.}
}
\author{
Miguel Alvarez, \email{malvarez@uni-bonn.de}
}
\examples{
## Add example for this function
}
