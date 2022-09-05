#' @name veg_diverstiy
#' @rdname veg_diversity
#'
#' @title Calculation of statistics at plot level
#'
#' @description
#' Calculation of diversity statistics at the plot level allowing for customized
#' functions defined as `foo(x, ...)`, where `x` is the vector of abundance
#' values.
#'
#' This function calls [taxa2samples()] to derive taxa from taxon usage names in
#' slot **samples** and multiple records of species in a single plot will be
#' merged by [stats::aggregate][aggregate()].
#'
#' The functions `shannon()`, `evenness()`, and `dominance()` calculate the
#' diversity index of Shannon, the evenness, and the dominance
#' (*1 - evenness*), respectively.
#' Dominance is the complementary value to evenness (i.e. `1 - evenness`).
#'
#' The function `simpson()` calculates the Simpson's index using the alternative
#' for vegetation plot observations.
#'
#' The function `richness()` counts the number of taxa per plot and can be used
#' as alternative to [vegtable::count_taxa][count_taxa()].
#'
#' @param x A numeric vector containing the abundance of single species.
#' @param na.rm A logical value indicating whether NA values should be removed
#'     from the abundance vector or not.
#' @param object A [vegtable-class] object.
#' @param weight A character value indicating the name of the column at slot
#'     **samples** which will be used as species abundance.
#' @param FUN A function used to calculate the diversity index.
#' @param aggr_fun A function used to aggregate abundance values for multiple
#'     records of a taxon in a plot observation. Average value is used by
#'     default.
#' @param arg_fun A named list with parameters and arguments passed to
#'     [taxa2samples()], which will
#'     retrieve the respective taxon concept for each taxon usage name and can
#'     be used to merge taxa at a determined taxonomic rank, for instance to
#'     merge all sub-specific taxa into their respective species
#'     (i.e. `'merge_to = "species"'`).
#' @param var_name A character value used as name for the calculated index.
#'     If missing, the name of the function will be used.
#' @param in_header A logical value indicating whether the results should be
#'     included as variables in the slot **header** of the input object.
#'     If `'in_header = TRUE'`, you may assign the result of the function to the
#'     input object.
#' @param ... Further arguments passed among methods. In `'evenness()'` and
#'     `'dominance()'`, these arguments are passed to `'shannon()'`.
#'     In `'veg_diversity()'`, these arguments are passed to [aggregate()]
#'     (actually to `'FUN'`).
#'
#' @return
#' Functions `shannon()`, `evenness()`, `dominance()`, `simpson()`, and
#' `richness()` return a numeric value (the calculated index).
#'
#' Funtion `veg_diversity()` produce either a data frame with calculated values
#' per plot observation (option `'in_header = FALSE'`) or a [vegtable-class]
#' object with the calculated values inserted in the slot **header**
#' (option `'in_header = TRUE'`).
#'
#' @examples
#' ## Compare Evenness with Shannon index
#' Kenya_veg <- cover_trans(x = Kenya_veg, to = "cover")
#' Kenya_veg <- veg_diversity(object = Kenya_veg, weight = "cover")
#' Kenya_veg <- veg_diversity(object = Kenya_veg, weight = "cover", FUN = evenness)
#'
#' with(Kenya_veg@header, plot(shannon, evenness))
#'
#' @aliases shannon
#' @export
shannon <- function(x, na.rm = TRUE, ...) {
  if (na.rm) x <- x[!is.na(x)]
  p <- x / sum(x)
  return(-sum(p * log(p)))
}

#' @aliases evenness
#' @rdname veg_diversity
#' @export
evenness <- function(x, ...) {
  return(shannon(x, ...) / log(length(x)))
}

#' @aliases dominance
#' @rdname veg_diversity
#' @export
dominance <- function(x, ...) {
  return(1 - evenness(x, ...))
}

#' @aliases simpson
#' @rdname veg_diversity
#' @export
simpson <- function(x, na.rm = TRUE, ...) {
  if (na.rm) x <- x[!is.na(x)]
  p <- x / sum(x)
  return(1 - sum(p^2))
}

#' @aliases richness
#' @rdname veg_diversity
#' @export
richness <- function(x, na.rm = TRUE, ...) {
  if (na.rm) x <- x[!is.na(x)]
  return(length(x))
}

#' @rdname veg_diversity
#' @export
veg_diversity <- function(object, ...) {
  UseMethod("veg_diversity", object)
}

#' @rdname veg_diversity
#' @aliases veg_diversity,vegtable-method
#' @method veg_diversity vegtable
#' @export
veg_diversity.vegtable <- function(object, weight, FUN = shannon,
                                   aggr_fun = mean, arg_fun = list(), var_name,
                                   in_header = TRUE,
                                   ...) {
  if (!weight %in% names(object@samples)) {
    stop(paste0(
      "The argument in 'weight' ('", weight,
      "') is not a column in slot 'samples'."
    ))
  }
  OUT <- do.call(taxa2samples, c(list(object = object), arg_fun))@samples
  OUT <- aggregate(as.formula(paste(weight, "TaxonConceptID + ReleveID",
    sep = " ~ "
  )), FUN = aggr_fun, data = OUT, na.action = na.omit)
  OUT <- aggregate(as.formula(paste(weight, "ReleveID", sep = " ~ ")),
    FUN = FUN, data = OUT, ...
  )
  if (missing(var_name)) {
    var_name <- as.character(substitute(FUN))
  }
  names(OUT)[names(OUT) == weight] <- var_name
  if (in_header) {
    object@header[[var_name]] <- with(
      OUT,
      get(var_name)[match(object$ReleveID, ReleveID)]
    )
    return(object)
  } else {
    return(OUT)
  }
}
