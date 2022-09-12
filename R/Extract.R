#' @name Extract
#' @aliases $ $,vegtable-method
#'
#' @title Select or replace elements in objects
#'
#' @description
#' Methods for quick access to slot `header` of [vegtable-class] objects or for
#' access to single cover scales in [coverconvert-class] objects.
#' Also replacement methods are implemented.
#'
#' @param x Object of class [vegtable-class].
#' @param ... Further arguments passed to or from other methods.
#' @param name A name to access.
#' @param i,j Indices for access.
#' @param drop A logical value passed to [Extract].
#' @param value Either a vectors or a list, used as replacement.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Range of latitude values in database
#' range(Kenya_veg$LATITUDE)
#'
#' ## Summary of countries
#' summary(Kenya_veg$COUNTRY)
#' summary(droplevels(Kenya_veg$COUNTRY))
#'
#' ## First 5 samples
#' summary(Kenya_veg[1:5, ])
#' @rdname Extract
#'
#' @exportMethod $
setMethod("$", signature(x = "vegtable"), function(x, name) {
  return(x@header[[name]])
})

#' @rdname Extract
#'
#' @aliases $<- $<-,vegtable-method
#'
#' @exportMethod $<-
#'
setReplaceMethod("$", signature(x = "vegtable"), function(x, name, value) {
  x@header[[name]] <- value
  return(x)
})

#' @rdname Extract
#' @aliases $,coverconvert-method
setMethod(
  "$", signature(x = "coverconvert"),
  function(x, name) {
    x@value <- x@value[name]
    x@conversion <- x@conversion[name]
    return(x)
  }
)

#' @rdname Extract
#' @aliases [ [,coverconvert,ANY,ANY,ANY-method [,coverconvert-method
#' @exportMethod [
setMethod("[", signature(x = "coverconvert"), function(x, i) {
  x@value <- x@value[i]
  x@conversion <- x@conversion[i]
  return(x)
})

#' @rdname Extract
#' @aliases $<-,coverconvert,coverconvert-method
setReplaceMethod(
  "$", signature(x = "coverconvert", value = "coverconvert"),
  function(x, name, value) {
    if (length(names(value)) > 1) {
      warning(paste0(
        "Multiple cover scales detected in 'value'. ",
        "Only the first ('", names(value)[1], "') will be inserted."
      ))
    }
    x@value[[name]] <- value@value[[1]]
    x@conversion[[name]] <- value@conversion[[1]]
    return(x)
  }
)

#' @rdname Extract
#' @aliases [ [,vegtable,ANY,ANY,ANY-method [,vegtable-method
#' @exportMethod [
setMethod("[", signature(x = "vegtable"), function(x, i, j, ..., drop = FALSE) {
  if (missing(i)) i <- TRUE
  if (missing(j)) j <- TRUE
  # Resolving problems with NAs
  if (is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
  if (is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
  x@header <- x@header[i, j, drop]
  # Subset on samples
  x@samples <- x@samples[x@samples$ReleveID %in% x@header$ReleveID, ]
  # Subset on species (same procedure as in import_vegtable)
  x <- clean(x)
  # Output
  return(x)
})

#' @rdname Extract
#' @aliases [<- [<-,vegtable-method
#' @exportMethod [<-
setReplaceMethod("[", signature(x = "vegtable"), function(x, i, j, value) {
  if (missing(i)) i <- TRUE
  if (missing(j)) j <- TRUE
  # Resolving problems with NAs
  if (is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
  if (is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
  x@header[i, j] <- value
  return(x)
})
