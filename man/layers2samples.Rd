\name{layers2samples}
\docType{methods}
\alias{layers2samples}
\alias{layers2samples,vegtable,character,character-method}
\alias{layers2samples,vegtable,character,missing-method}

\title{Add information from slot 'layers' into slot 'samples'.}
\description{
Slot layers may include additional information that should be moved to samples
in order to use it by \code{\link[vegtable]{subset}},
\code{\link[vegtable]{aggregate}} or \code{\link[vegtable]{crosstable}} methods.
}
\usage{
\S4method{layers2samples}{vegtable,character,character}(object, layer,
	variable, ...)
\S4method{layers2samples}{vegtable,character,missing}(object, layer,
	variable, ...)
}
\arguments{
\item{object}{An object of class \code{\linkS4class{vegtable}}.}
\item{layer}{Character value indicating a target layer.}
\item{variable}{Character vector with the names of variables to be inserted in
	slot \code{'samples'}.}
\item{...}{Further arguments to be passed among methods.}
}
\details{
If names of variables are not provided, all variables from the respective layer
table will be inserted in slot \code{'samples'}.
}
\value{
An object of class \code{\linkS4class{vegtable}} with variables added to
samples.
}
\author{
Miguel Alvarez (\email{kamapu78@gmail.com}).
}
\examples{
## No example available for this function.
}
