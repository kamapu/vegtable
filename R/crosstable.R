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
#' @param level A character vector with taxonomic ranks (levels) requested in
#'     the cross table.
#' @param include_lower A logical value indicating wether lower value to the
#'     requested levels should be merged or not. It works only if `'level'` is
#'     not missing. Note that if you like to include higher ranks or rankless
#'     taxa in the cross table, you will rahter need to run
#'     [taxlist::merge_taxa()] on slot **species**.
#' @param na_to_zero A logical value indicating whether zeros should be
#'     inserted into empty cells or not.
#' @param use_nas Logical value indicating whether NAs should be considered as
#'     levels for categorical variables or not.
#' @param as_matrix A logical value, whether output should be done as matrix or
#'     data frame.
#' @param terms A character vector with the names of columns used by
#'     `cross2db()` as species and layers.
#' @param ... Further arguments passed to the function [stats::aggregate()].
#' @param object A data frame or a matrix including a cross table. Note that
#'     `cross2db()` assumes observations as columns and species (and layers)
#'     as rows in the `data.frame-method` but species as columns and
#'     observations as rows in the `matrix-method`.
#' @param layers Logical value, whether the cross table includes a layer column
#'     besides the species column or not. This apply for `cross2db()` and will
#'     be ignored if 'terms' are provided.
#' @param na_strings Character vector indicating no records in the cross table.
#' @param na.rm A logical value indicating to `cross2db()` whether empty cells
#'     should be included in the database list or not.
#' @param split_cover A character value showing a symbol used to split the
#'     column cover by `cross2db()`. This is used in the case that a vegetation
#'     table includes abundance and sociability in a same value.
#'
#' @return An object of class [data.frame].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @example examples/crosstable.R
#'
#' @rdname crosstable
#'
#' @exportMethod crosstable
#'
setGeneric("crosstable", function(formula, data, ...) {
  standardGeneric("crosstable")
})

#' @rdname crosstable
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
#' @aliases crosstable,formula,vegtable-method
setMethod(
  "crosstable", signature(formula = "formula", data = "vegtable"),
  function(formula, data, FUN, level, include_lower = FALSE, na_to_zero = FALSE,
           use_nas = TRUE, ...) {
    Terms <- c(as.character(formula)[2], attr(terms(formula), "term.labels"))
    # If ranks and merge required
    if (!missing(level)) {
      if (include_lower) {
        data@species <- merge_taxa(data@species,
          level = level,
          delete_nomatch = TRUE
        )
      } else {
        data@species@taxonRelations <- data@species@taxonRelations[
          data@species@taxonRelations$Level %in% level,
        ]
        data@species <- clean(data@species)
        data <- clean(data)
      }
    }
    # The rest works as usual
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
      stop(paste(
        "Terms 'TaxonName' and 'AcceptedName'",
        "are mutually exclusive in 'formula'"
      ))
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
#' @aliases cross2db
#' @export
cross2db <- function(object, ...) {
  UseMethod("cross2db", object)
}

#' @rdname crosstable
#' @aliases cross2db,data.frame-method
#' @export
cross2db.data.frame <- function(
  object, layers = FALSE, na_strings,
  terms, na.rm = TRUE, split_cover, ...
) {
  # Set the terms
  if (missing(terms)) {
    if (layers) {
      terms <- names(object)[1:2]
    } else {
      terms <- names(object)[1]
    }
  }
  # Check terms
  if (!all(terms %in% names(object))) {
    missing_terms <- terms[!terms %in% names(object)]
    stop(paste0(
      "Following terms are not in the column names of 'object':\n  ",
      paste0(missing_terms, collapse = ", ")
    ))
  }
  # Names of plots
  plots <- names(object)[!names(object) %in% terms]
  N <- length(terms)
  # Extract
  db_list <- lapply(plots, function(i) {
    df <- object[, c(terms, i)]
    names(df)[N + 1] <- "cover"
    df$plot <- i
    df
  })
  db_list <- do.call(rbind, db_list)[, c(terms, "plot", "cover")]
  # Handle NAs
  if (!missing(na_strings)) {
    db_list$cover[paste(db_list$cover) %in% na_strings] <- NA
  }
  if (na.rm) {
    db_list <- db_list[!is.na(db_list$cover), ]
  }
  # Split cover
  if (!missing(split_cover)) {
    db_list[c("cover1", "cover2")] <- do.call(
      rbind,
      strsplit(db_list$cover, split_cover, fixed = TRUE)
    )
    db_list$cover <- NULL
  }
  db_list
}

#' @rdname crosstable
#' @aliases cross2db,matrix-method
#' @export
cross2db.matrix <- function(object, ...) {
  object <- as.data.frame(t(object))
  object <- cbind(data.frame(species = rownames(object)), object)
  cross2db(object, ...)
}
