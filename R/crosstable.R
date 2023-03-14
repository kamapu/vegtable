#' @name crosstable
#'
#' @title Generating cross tables from database lists
#'
#' @description
#' cross table is the most common format required by statistical packages
#' used to analyse vegetation data (e.g.
#' [vegan](https://CRAN.R-project.org/package=vegan)).
#'
#' You may use for convenience a formula as
#' `'abundance ~ plot + species + ...'`.
#' Additional variables used for rows (`...`) can be for instance the
#' layers.
#' For objects of class [vegtable-class], the formula can also include
#' variables from the species list (for example `AcceptedName`, `AuthorName`)
#' or even taxon traits.
#'
#' If required, tables already formatted as cross tables can be converted into
#' column-oriented tables by using the function `cross2db()`.
#'
#' @param formula A formula indicating the variables used in the cross table.
#'     This formula can be represented as `'abundance ~ cols + rows'`, where
#'     `'abundance'` is the numeric variable quantified for a row in a column,
#'     for instance the abundance of a species in a plot.
#'     Further variables can be set as additional rows indices in a cross table.
#' @param data Either a data frame or an object of class [vegtable-class].
#' @param FUN Function used to aggregate values in the case of a multiple
#'     occurrence of a species in a plot, for instance.
#' @param na_to_zero A logical value indicating whether zeros should be
#'     inserted into empty cells or not.
#' @param use_nas Logical value indicating whether NAs should be considered as
#'     levels for categorical variables or not.
#' @param as_matrix A logical value, whether output should be done as matrix or
#'     data frame.
#' @param ... Further arguments passed to the function [stats::aggregate()].
#' @param object A data frame including a cross table.
#' @param layers Logical value, whether the cross table includes a layer column
#'     or not.
#' @param na_strings Character vector indicating no records in the cross table.
#'
#' @return An object of class [data.frame].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' veg <- subset(Kenya_veg, REFERENCE == 2331, slot = "header")
#'
#' ## transform cover to percentage
#' veg <- cover_trans(veg, to = "cover_perc", rule = "middle")
#'
#' ## cross table of the first 5 plots
#' Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
#'   veg[1:5, ], mean,
#'   na_to_zero = TRUE
#' )
#' head(Cross)
#' @rdname crosstable
#'
#' @exportMethod crosstable
#'
setGeneric("crosstable", function(formula, data, ...) {
  standardGeneric("crosstable")
})

#' @rdname crosstable
#'
#' @aliases crosstable,formula,data.frame-method
setMethod(
  "crosstable", signature(formula = "formula", data = "data.frame"),
  function(formula, data, FUN, na_to_zero = FALSE, use_nas = TRUE,
           as_matrix = FALSE, ...) {
    if (!all(c(as.character(formula)[2], attr(
      terms(formula),
      "term.labels"
    )) %in%
      colnames(data))) {
      stop("all terms in 'formula' must be a column in 'data'")
    }
    if (use_nas) {
      Terms <- c(as.character(formula)[2], attr(
        terms(formula),
        "term.labels"
      ))
      for (i in Terms[-1]) {
        if (is.factor(data[, i])) {
          data[, i] <- paste(data[, i])
        }
        if (is.character(data[, i])) {
          data[is.na(data[, i]), i] <- ""
        }
      }
    }
    data <- aggregate(formula, data, FUN, ...)
    coverage <- as.character(formula)[2]
    plots <- attr(terms(formula), "term.labels")[1]
    # for multiple plot entries
    if (length(attr(terms(formula), "term.labels")) > 2) {
      data$.spp <- apply(
        data[attr(
          terms(formula),
          "term.labels"
        )[-1]], 1, paste,
        collapse = "."
      )
      spp <- ".spp"
    } else {
      spp <- attr(terms(formula), "term.labels")[2]
    }
    cross <- expand.grid(unique(data[, spp]), unique(data[, plots]),
      stringsAsFactors = FALSE
    )
    colnames(cross) <- c(spp, plots)
    cross[, coverage] <- data[match(
      paste(cross[, spp], cross[, plots]),
      paste(data[, spp], data[, plots])
    ), coverage]
    if (na_to_zero) cross[is.na(cross[, coverage]), coverage] <- 0
    cross <- matrix(cross[, coverage],
      ncol = length(unique(cross[, plots])),
      dimnames = list(unique(cross[, spp]), unique(cross[, plots]))
    )
    # final output data frame
    if (spp == ".spp") {
      cross_margin <- unique(data[c(".spp", attr(
        terms(formula),
        "term.labels"
      )[-1])])
      cross_margin <- cross_margin[match(
        rownames(cross),
        cross_margin$.spp
      ), colnames(cross_margin)[-1]]
    } else {
      cross_margin <- data.frame(unique(data[, spp]),
        stringsAsFactors = FALSE
      )
      colnames(cross_margin) <- spp
    }
    if (!as_matrix) {
      cross <- do.call(cbind, list(cross_margin, cross))
      rownames(cross) <- NULL # reseting row names
    }
    return(cross)
  }
)

#' @rdname crosstable
#'
#' @aliases crosstable,formula,vegtable-method
setMethod(
  "crosstable", signature(formula = "formula", data = "vegtable"),
  function(formula, data, FUN, na_to_zero = FALSE, use_nas = TRUE, ...) {
    Terms <- c(as.character(formula)[2], attr(
      terms(formula),
      "term.labels"
    ))
    data@samples <- data@samples[, colnames(data@samples) %in%
      c("ReleveID", "TaxonUsageID", Terms)]
    # Data from species
    data@samples$TaxonConceptID <- data@species@taxonNames[
      match(
        data@samples$TaxonUsageID,
        data@species@taxonNames$TaxonUsageID
      ),
      "TaxonConceptID"
    ]
    if ("TaxonName" %in% Terms & "AcceptedName" %in% Terms) {
      stop("Terms 'TaxonName' and 'AcceptedName' are mutually exclusive in 'formula'")
    }
    # 1: when usage name requested
    if ("TaxonName" %in% Terms) {
      data@samples$TaxonName <- data@species@taxonNames[
        match(
          data@samples$TaxonUsageID,
          data@species@taxonNames$TaxonUsageID
        ),
        "TaxonName"
      ]
      data@samples$AuthorName <- data@species@taxonNames[
        match(
          data@samples$TaxonUsageID,
          data@species@taxonNames$TaxonUsageID
        ),
        "AuthorName"
      ]
    }
    # 2: when accepted name requested
    if ("AcceptedName" %in% Terms) {
      data@samples$AcceptedName <- data@species@taxonRelations[
        match(
          data@samples$TaxonConceptID,
          data@species@taxonRelations$TaxonConceptID
        ),
        "AcceptedName"
      ]
      data@samples$AuthorName <- data@species@taxonNames[
        match(
          data@samples$AcceptedName,
          data@species@taxonNames$TaxonUsageID
        ),
        "AuthorName"
      ]
      data@samples$AcceptedName <- data@species@taxonNames[
        match(
          data@samples$AcceptedName,
          data@species@taxonNames$TaxonUsageID
        ),
        "TaxonName"
      ]
    }
    # Data from traits (only for Accepted Name or TaxonConceptID)
    traits_names <- colnames(data@species@taxonTraits)[
      colnames(data@species@taxonTraits) != "TaxonConceptID"
    ]
    if (any(Terms %in% traits_names)) {
      traits_names <- traits_names[traits_names %in% Terms]
      for (i in traits_names) {
        data@samples[, i] <- data@species@taxonTraits[
          match(
            data@samples$TaxonConceptID,
            data@species@taxonTraits$TaxonConceptID
          ), i
        ]
      }
    }
    # Data from header
    header_names <- colnames(data@header)[colnames(data@header) !=
      "ReleveID"]
    if (any(header_names %in% Terms)) {
      header_names <- header_names[header_names %in% Terms]
      for (i in header_names) {
        data@samples[, i] <- data@header[match(
          data@samples$ReleveID,
          data@header$ReleveID
        ), i]
      }
    }
    # Continue with method data.frame
    return(crosstable(
      formula, data@samples, FUN, na_to_zero, use_nas,
      ...
    ))
  }
)

#' @rdname crosstable
#'
#' @aliases cross2db
#'
#' @export
cross2db <- function(object, layers = FALSE, na_strings) {
  species <- object[, 1]
  if (layers) {
    LAY <- object[, 2]
    Cover <- object[, -1:-2]
    object <- lapply(split(1:ncol(Cover), 1:ncol(Cover)),
      function(x, cov, spec, lay) {
        releve <- data.frame(
          plot = colnames(cov)[x], species = spec,
          layers = lay, cover = cov[, x], stringsAsFactors = FALSE
        )
        return(releve)
      },
      cov = Cover, spec = species, lay = LAY
    )
  } else {
    Cover <- object[, -1, drop = FALSE]
    object <- lapply(split(1:ncol(Cover), 1:ncol(Cover)),
      function(x, cov, spec) {
        releve <- data.frame(
          plot = colnames(cov)[x], species = spec,
          cover = cov[, x], stringsAsFactors = FALSE
        )
        return(releve)
      },
      cov = Cover, spec = species
    )
  }
  object <- do.call(rbind, object)
  if (!missing(na_strings)) {
    object$cover[paste(object$cover) %in% na_strings] <- NA
  }
  return(object[!is.na(object$cover), ])
}
