#' @name count_taxa
#' @aliases count_taxa,vegtable,missing-method
#'
#' @title Count taxa included in vegtable objects
#'
#' @description
#' Counting number of taxa within [taxlist-class] objects or character vectors
#' containing taxon names.
#'
#' This function provides a quick calculation of taxa in [vegtable-class]
#' objects, considering only records in slot samples.
#' Such records can be also merged from lower ranks.
#'
#' For the formula method, units without any requested taxa will not appear in
#' the output data frame. If no taxa at all is occurring at the requested level
#' in any unit, an error message will be retrieved.
#'
#' @param object An object of class [vegtable-class] or a formula.
#' @param value A formula passed to parameter 'object' by the replace method.
#' @param data An object of class [vegtable-class].
#' @param level Character value indicating the taxonomic rank of counted taxa.
#' @param include_lower Logical value, whether lower taxonomic ranks should be
#'     included at the requested level.
#' @param suffix Character value used as suffix on the calculated variable.
#' @param in_header Logical value, whether the result should be included in the
#'     slot header of the input [vegtable-class] object or not.
#'     If the formula term is related to a categorical variable at header, the
#'     result will be inserted in the respective table at slot **relations**.
#' @param ... further arguments passed among methods.
#'
#' @return
#' An data frame with the number of taxa from requested level at requested
#' units for the formula method, or just an integer value.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Different alternatives
#' count_taxa(Kenya_veg)
#' head(count_taxa(~ReleveID, Kenya_veg, in_header = FALSE))
#' head(count_taxa(species ~ ReleveID, Kenya_veg, in_header = FALSE))
#' head(count_taxa(species ~ ReleveID, Kenya_veg, TRUE, in_header = FALSE))
#' head(count_taxa(family ~ ReleveID, Kenya_veg, TRUE))
#'
#' @rdname count_taxa
#'
#' @exportMethod count_taxa
#'
setMethod(
  "count_taxa", signature(object = "vegtable", data = "missing"),
  function(object, level, include_lower = FALSE, ...) {
    concepts <- with(
      object@species@taxonNames,
      TaxonConceptID[match(
        object@samples$TaxonUsageID,
        TaxonUsageID
      )]
    )
    if (!missing(level)) {
      if (!level %in% levels(object@species)) {
        stop(paste(
          "Value of argument 'level' is not a level",
          "in 'object'."
        ))
      }
    }
    if (!missing(level) & include_lower) {
      concept_levels <- with(
        object@species@taxonRelations,
        as.integer(Level)[match(concepts, TaxonConceptID)]
      )
      # Skip NA's from taxon levels
      concepts <- concepts[!is.na(concept_levels)]
      concept_levels <- concept_levels[!is.na(concept_levels)]
      x <- which(levels(object@species) == level) - 1
      for (i in 1:x) {
        concepts[concept_levels == i] <-
          with(
            object@species@taxonRelations,
            Parent[match(
              concepts[concept_levels == i],
              TaxonConceptID
            )]
          )
        concept_levels <- with(
          object@species@taxonRelations,
          as.integer(Level)[match(concepts, TaxonConceptID)]
        )
      }
    }
    if (!missing(level)) {
      concept_levels <- with(
        object@species@taxonRelations,
        paste(Level)[match(concepts, TaxonConceptID)]
      )
      concepts <- concepts[concept_levels == level]
    }
    return(length(unique(concepts)))
  }
)

#' @rdname count_taxa
#' @aliases count_taxa,formula,vegtable-method
setMethod(
  "count_taxa", signature(object = "formula", data = "vegtable"),
  function(object, data, include_lower = FALSE, suffix = "_count",
           in_header = TRUE, ...) {
    data_in <- data
    nr_response <- attr(terms(object), "response")
    name_response <- as.character(object)[2]
    f_term <- attr(terms(object), "term.labels")
    if (in_header) {
      if (length(f_term) > 1) {
        stop(paste(
          "Only one term is allowed in formula for the option",
          "'in_header = TRUE'."
        ))
      }
      if (!f_term %in% names(data@header)) {
        stop(paste(
          "Term in formula is not a variable at slot header",
          "as required for 'in_header = TRUE'."
        ))
      }
    }
    if (nr_response == 1) {
      if (!name_response %in% levels(data@species)) {
        stop("The response in the formula is not a rank in 'data'.")
      }
      object <- as.formula(paste("TaxonConceptID ~", paste(f_term,
        collapse = " + "
      )))
      if (include_lower) {
        data <- taxa2samples(data,
          merge_to = name_response,
          include_levels = name_response, add_relations = TRUE
        )
      } else {
        data <- taxa2samples(data,
          include_levels = name_response,
          add_relations = TRUE
        )
      }
      if (all(is.na(data@samples$TaxonConceptID))) {
        stop("No records for requested taxon rank.")
      }
    } else {
      data <- taxa2samples(data, add_relations = TRUE)
      object <- as.formula(paste("TaxonUsageID ~", paste(f_term,
        collapse = " + "
      )))
    }
    head_vars <- f_term[f_term %in% names(data_in@header)[
      names(data_in@header) != "ReleveID"
    ]]
    if (length(head_vars) > 0) {
      for (i in head_vars) {
        data@samples[[i]] <- data@header[[i]][
          match(data@samples$ReleveID, data@header$ReleveID)
        ]
      }
    }
    data <- aggregate(object, data@samples, function(x) length(unique(x)), ...)
    if (nr_response == 0) {
      name_response <- paste0("taxa", suffix)
    } else {
      name_response <- paste0(name_response, suffix)
    }
    colnames(data)[colnames(data) %in% c("TaxonUsageID", "TaxonConceptID")] <-
      name_response
    if (in_header) {
      if (f_term == "ReleveID") {
        data_in@header[[name_response]] <- data[[name_response]][
          match(data_in@header$ReleveID, data$ReleveID)
        ]
      } else {
        if (!f_term %in% names(data_in@relations)) {
          new_relation(data_in) <- f_term
        }
        data_in@relations[[f_term]][[name_response]] <- data[[name_response]][
          match(data_in@relations[[f_term]][[f_term]], data[[f_term]])
        ]
      }
      return(data_in)
    } else {
      return(data)
    }
  }
)

#' @rdname count_taxa
#' @aliases count_taxa<-
#' @exportMethod count_taxa<-
setGeneric("count_taxa<-", function(data, ..., value) {
  standardGeneric("count_taxa<-")
})

#' @rdname count_taxa
#' @aliases count_taxa<-,vegtable,formula-method
setReplaceMethod(
  "count_taxa", signature(data = "vegtable", value = "formula"),
  function(data, ..., value) {
    return(count_taxa(
      object = value, data = data, in_header = TRUE,
      ...
    ))
  }
)
