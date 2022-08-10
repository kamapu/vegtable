#' @name taxa2samples
#'
#' @title Insert taxon information into samples
#'
#' @description
#' For statistical purposes it may be necessary to insert information on
#' recorded taxa into the slot samples, which contain the records of taxa in
#' sampling plots.
#' This can be also done selectivelly for specific taxonomic ranks and lower
#' ranks can be aggregated to their parental ones.
#'
#' @param object A [vegtable-class] object.
#' @param merge_to Character value indicating the level (taxonomic rank) to
#'     which taxa of lower rank have to be merged.
#' @param include_levels Character vector indicating the levels to be considered
#'     in the output object. This will set the values of **TaxonConceptID** and
#'     any respective values inserted from slots **taxonRelations** and
#'     **taxonTraits** as NA.
#' @param add_relations A logical value indicating whether the content of slot
#'     **taxonRelations** have to be inserted in slot **samples** or not.
#' @param add_traits A logical value indicating whether the content of slot
#'     **taxonTraits** have to be inserted in slot **samples** or not.
#' @param ... Further arguments passed among methods.
#'
#' @return An object of class [vegtable-class].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Add only variable TaxonConceptID
#' veg <- taxa2samples(Kenya_veg)
#' head(veg@samples)
#'
#' ## Add also information from slots taxonRelations and taxonTraits
#' veg <- taxa2samples(Kenya_veg, add_relations = TRUE, add_traits = TRUE)
#' head(veg@samples)
#'
#' ## Different ranks recorded at samples
#' veg <- taxa2samples(Kenya_veg, add_relations = TRUE)
#' summary(veg@samples$Level)
#' 
#' ## Aggregate taxa to family level
#' veg <- taxa2samples(Kenya_veg, add_relations = TRUE, merge_to = "family")
#' summary(veg@samples$Level)
#'
#' @rdname taxa2samples
#'
#' @aliases taxa2samples
#'
#' @export taxa2samples
taxa2samples <- function(object, ...) {
  UseMethod("taxa2samples", object)
}

#' @rdname taxa2samples
#' @aliases taxa2samples,vegtable-method
#' @method taxa2samples vegtable
#' @export
taxa2samples.vegtable <- function(object, merge_to, include_levels,
                                  add_relations = FALSE, add_traits = FALSE,
                                  ...) {
  # Internal objects
  spp <- used_concepts(object, keep_children = TRUE, keep_parents = TRUE)
  samples <- data.frame(TaxonUsageID = unique(object@samples$TaxonUsageID))
  samples$TaxonConceptID <- spp@taxonNames$TaxonConceptID[
    match(samples$TaxonUsageID, spp@taxonNames$TaxonUsageID)
  ]
  # Merge to level
  if (!missing(merge_to)) {
    if (!merge_to[1] %in% levels(spp)) {
      stop(paste(
        "Value of argument 'merge_to' is not a level in",
        "'object'."
      ))
    }
    x <- which(levels(spp) == merge_to[1]) - 1
    for (i in 1:x) {
      Level <- spp@taxonRelations$Level[match(
        samples$TaxonConceptID,
        spp@taxonRelations$TaxonConceptID
      )]
      concept <- unique(samples$TaxonConceptID[as.integer(Level) == i &
        !is.na(Level)])
      if (length(concept) > 0) {
        concept_parent <- spp@taxonRelations$Parent[match(
          concept,
          spp@taxonRelations$TaxonConceptID
        )]
        samples$TaxonConceptID <- replace_x(samples$TaxonConceptID,
          old = concept, new = concept_parent
        )
      }
    }
  }
  # Select some ranks
  if (!missing(include_levels)) {
    no_level <- include_levels[!include_levels %in% levels(spp)]
    if (length(no_level) > 0) {
      stop(paste0(
        "These values of 'include_levels' are not levels ",
        "at slot 'species':\n'", paste0(no_level, collapse = "', '"),
        "'"
      ))
    }
    Level <- spp@taxonRelations$Level[match(
      samples$TaxonConceptID,
      spp@taxonRelations$TaxonConceptID
    )]
    samples <- samples[paste(Level) %in% include_levels & !is.na(Level), ]
  }
  if (add_relations) {
    samples <- merge(samples, spp@taxonRelations, sort = FALSE)
  }
  if (add_traits) {
    samples <- merge(samples, spp@taxonTraits, sort = FALSE, all.x = TRUE)
  }
  object@samples <- merge(object@samples, samples,
    by = "TaxonUsageID",
    sort = FALSE, all.x = TRUE
  )
  return(object)
}
