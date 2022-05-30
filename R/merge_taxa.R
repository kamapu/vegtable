#' @name merge_taxa
#'
#' @title Merge concepts
#'
#' @description
#' Merge taxon concepts form into single ones or insert accepted names to slot
#' samples.
#'
#' This method is applied to a function defined in the package [taxlist-package]
#' and only modify the slot `species` in the input `object`.
#'
#' The use of `taxa2samples()` with `merge_to` argument will produce a similar
#' result as using `merge_taxa` with `level` argument, but `taxa2samples()`
#' will replace the records in slot samples by the respective accepted names
#' without any modification in slot species.
#' Additionally taxon concept IDs will be addes as columns in samples and taxon
#' traits if indicated in argument `add_traits`.
#'
#' @param object Object of class [vegtable-class].
#' @param concepts Numeric (integer) vector including taxon concepts to be
#'     merged.
#' @param level,merge_to Character value indicating the level to which the taxa
#'     have to be merged.
#' @param include_levels Character vector indicating the levels to be considered
#'     in the output object. It can be used to exclude some taxonomic ranks.
#' @param na.rm Logical value. Apply to records with missing information on
#'     taxonomic rank (i.e. for undetermined specimens).
#' @param add_traits A character vector indicating variables in the slot
#'     `taxonTraits` to be added in slot `samples`.
#' @param ... Further arguments passed to [taxlist::merge_taxa()].
#'
#' @return An object of class [vegtable-class].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Merge Olea capensis into one
#' summary(subset(Kenya_veg@species, grepl("Olea capensis", TaxonName),
#'   slot = "names"
#' ), "all")
#' Kenya_veg <- merge_taxa(Kenya_veg, c(52041, 50432, 50235))
#'
#' ## Check Olea capensis again
#' summary(subset(Kenya_veg@species, grepl("Olea capensis", TaxonName),
#'   slot = "names"
#' ), "all")
#'
#' ## Effect of taxa2samples by counting taxa
#' count_taxa(Kenya_veg, level = "genus")
#'
#' Kenya_veg <- taxa2samples(Kenya_veg, merge_to = "genus")
#' count_taxa(Kenya_veg, level = "genus")
#' @rdname merge_taxa
#'
#' @aliases merge_taxa,vegtable,numeric,missing-method
#'
#' @exportMethod merge_taxa
#'
setMethod(
  "merge_taxa", signature(
    object = "vegtable", concepts = "numeric",
    level = "missing"
  ),
  function(object, concepts, ...) {
    object@species <- merge_taxa(object@species, concepts, ...)
    return(object)
  }
)

#' @rdname merge_taxa
#'
#' @aliases merge_taxa,vegtable,missing,missing-method
setMethod(
  "merge_taxa", signature(
    object = "vegtable", concepts = "missing",
    level = "character"
  ),
  function(object, level, ...) {
    object@species <- merge_taxa(object@species, level = level, ...)
    return(object)
  }
)

#' @rdname merge_taxa
#'
#' @aliases taxa2samples
#'
#' @exportMethod taxa2samples
#'
setGeneric(
  "taxa2samples",
  function(object, ...) {
    standardGeneric("taxa2samples")
  }
)

#' @rdname merge_taxa
#'
#' @aliases taxa2samples,vegtable-method
setMethod(
  "taxa2samples", signature(object = "vegtable"),
  function(object, merge_to, include_levels, na.rm = FALSE, add_traits,
           ...) {
    # Add Taxon Concepts
    object@samples$TaxonConceptID <-
      object@species@taxonNames$TaxonConceptID[match(
        object@samples$TaxonUsageID,
        object@species@taxonNames$TaxonUsageID
      )]
    # Add levels
    # TODO: Check for objects without ranks
    object@samples$Level <-
      object@species@taxonRelations$Level[match(
        object@samples$TaxonConceptID,
        object@species@taxonRelations$
          TaxonConceptID
      )]
    # Merge to level
    if (!missing(merge_to)) {
      if (!merge_to[1] %in% levels(object@species)) {
        stop(paste(
          "Value of argument 'merge_to' is not a level in",
          "'object'."
        ))
      }
      x <- which(levels(object@species) == merge_to[1]) - 1
      for (i in 1:x) {
        concept <- unique(object@samples$TaxonConceptID[
          as.integer(object@samples$Level) == i &
            !is.na(object@samples$Level)
        ])
        if (length(concept) > 0) {
          concept_parent <- object@species@taxonRelations$
            Parent[match(concept, object@species@
          taxonRelations$
            TaxonConceptID)]
          object@samples$TaxonConceptID <- replace_x(
            object@samples$TaxonConceptID,
            old = concept, new = concept_parent
          )
        }
        # Add levels
        # TODO: Check for objects without ranks
        object@samples$Level <-
          object@species@taxonRelations$Level[match(
            object@samples$TaxonConceptID,
            object@species@taxonRelations$
              TaxonConceptID
          )]
      }
    }
    # Add Accepted Names
    object@samples$TaxonUsageID <- object@species@taxonRelations$
      AcceptedName[match(
      object@samples$TaxonConceptID,
      object@species@taxonRelations$
        TaxonConceptID
    )]
    # Remove NAs?
    if (na.rm) {
      object@samples <- object@samples[!is.na(object@samples$Level), ]
    }
    # Select some ranks
    if (!missing(include_levels)) {
      if (any(!include_levels %in% levels(object@species))) {
        stop(paste(
          "Some values in 'include_levels' are not",
          "levels in slot 'species'."
        ))
      }
      object@samples <- object@samples[
        paste(object@samples$Level) %in% include_levels &
          !is.na(object@samples$Level),
      ]
    }
    # Add traits?
    if (!missing(add_traits)) {
      if (!any(add_traits %in% colnames(object@species@taxonTraits))) {
        stop(paste(
          "Values in 'add_traits' are not present as",
          "variable in the slot 'taxonTraits'."
        ))
      }
      if (any(!add_traits %in% colnames(object@species@taxonTraits))) {
        warning(paste(
          "Some requested taxon traits are missing",
          "and will not appear in the output."
        ))
      }
      for (i in add_traits) {
        object@samples[, i] <- NA
        object@samples[, i] <- with(
          object@species@taxonTraits,
          replace_idx(
            object@samples[, i],
            object@samples$TaxonConceptID,
            TaxonConceptID, get(i)
          )
        )
      }
    }
    return(object)
  }
)
