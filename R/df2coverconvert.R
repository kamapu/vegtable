#' @name df2coverconvert
#'
#' @title Create coverconvert objects
#'
#' @description
#' The class [coverconvert-class] contains tables for transforming cover values
#' to percentage using the function [cover_trans()].
#' These objects can be created from conversion tables imported as data frames.
#'
#' @param x Either a data frame or a list of data frames containing the
#'     conversion table. Three columns are mandatory in such data frames, namely
#'     **value** (factor with the symbols for each class in the cover scale,
#'     sorted from the lowest to the highest value), **bottom** (numeric value
#'     with the bottom values of each class), and **top** (numeric value with
#'     the top values of each class). The values **bottom** and **top** are
#'     usually as cover percentage but they may refer to any other numeric
#'     abundance.
#' @param name A character value used as name of the cover scale in the data
#'     frame method. In the list method, this name will be extracted from the
#'     names of the elements in the list.
#' @param ... Further arguments passed among methods.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Convert object into list
#' cov <- as(Kenya_veg@coverconvert, "list")
#'
#' ## Convert back to coverconvert
#' cov <- df2coverconvert(cov)
#'
#' @rdname df2coverconvert
#'
#' @export
df2coverconvert <- function(x, ...) {
  UseMethod("df2coverconvert", x)
}

#' @rdname df2coverconvert
#' @aliases df2coverconvert,list-method
#' @method df2coverconvert list
#' @export
df2coverconvert.list <- function(x, ...) {
  x_names <- c("value", "bottom", "top")
  OUT <- new("coverconvert")
  for (i in names(x)) {
    if (!all(x_names %in% names(x[[i]]))) {
      x_names <- x_names[!x_names %in% names(x[[i]])]
      stop(paste0(
        "Following mandatory columns are missing in cover scale '",
        i, "': '", paste0(x_names, collapse = "', '"), "'."
      ))
    }
    OUT@value[[i]] <- factor(x[[i]]$value, levels = x[[i]]$value)
    OUT@conversion[[i]] <- c(x[[i]]$bottom[1], x[[i]]$top)
  }
  return(OUT)
}

#' @rdname df2coverconvert
#' @aliases df2coverconvert,data.frame-method
#' @method df2coverconvert data.frame
#' @export
df2coverconvert.data.frame <- function(x, name, ...) {
  if (missing(name)) {
    stop("You have to provide a 'name' for this cover scale.")
  }
  x2 <- list()
  x2[[name]] <- x
  return(df2coverconvert(x2))
}
