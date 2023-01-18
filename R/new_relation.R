#' @name new_relation
#' @rdname new_relation
#'
#' @title Insert a new variable as relation in vegtable object
#'
#' @description
#' Insert a new variable in slot **header** with a respective table at slot
#' **relations**. The respective variable in header will be set as factor.
#'
#' Existing categorical variables can also be set as relations. If such
#' variables are factors, its levels can be preserved (missing argument in
#' `'levels'`) or reset.
#'
#' @param object A [vegtable-class] object.
#' @param relation,value A character value indicating the name of the new
#'     relation. The parameter 'value' is used for the replacement method
#' @param levels A character vector with the levels for the inserted factor.
#'     This may be missing for variables that already exist in slot **header**.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' A [vegtable-class] object with the inserted new relation.
#'
#' @examples
#' ## A brand new variable
#' new_relation(Kenya_veg, levels = c("forest", "grassland", "cropland")) <- "land_use"
#'
#' ## Set an existing variable as relation
#' new_relation(Kenya_veg) <- "REMARKS"
#'
#' @export
new_relation <- function(object, ...) {
  UseMethod("new_relation", object)
}

#' @rdname new_relation
#' @aliases new_relation,vegtable-method
#' @method new_relation vegtable
#' @export
new_relation.vegtable <- function(object, relation, levels, ...) {
  # Conditions
  if (length(relation) > 1) {
    warning(paste(
      "Argument in 'relation' is longer than 1.",
      "Only the first value will be used."
    ))
    relation <- relation[1]
  }
  if (relation %in% names(object@relations)) {
    stop(paste0("Relation '", relation, "' is already in 'object'."))
  }
  # New relation already in header
  if (relation %in% names(object@header)) {
    if (is(object@header[, relation], "factor") & missing(levels)) {
      levels <- base::levels(object@header[, relation])
      relation_vector <- factor(
        unique(as.character(object@header[
          !is.na(object@header[, relation]), relation
        ])),
        levels = levels
      )
    } else {
      if (missing(levels)) {
        relation_vector <- as.factor(unique(as.character(object@header[
          !is.na(object@header[, relation]), relation
        ])))
        object@header[, relation] <-
          factor(as.character(object@header[, relation]),
            levels = base::levels(relation_vector)
          )
      } else {
        relation_vector <- factor(
          unique(as.character(object@header[
            !is.na(object@header[, relation]), relation
          ])),
          levels = levels
        )
        object@header[, relation] <-
          factor(as.character(object@header[, relation]),
            levels = levels
          )
      }
    }
  } else {
    if (missing(levels)) {
      stop(paste(
        "For new relations you have to provide",
        "an argument for 'levels'."
      ))
    } else {
      relation_vector <- factor(levels, levels = levels)
      object@header[, relation] <- factor(NA, levels = levels)
    }
  }
  # Insert relation_vector into slot
  object@relations[[relation]] <- list()
  object@relations[[relation]][[relation]] <- relation_vector
  object@relations[[relation]] <- as.data.frame(object@relations[[relation]])
  return(object)
}

#' @rdname new_relation
#' @aliases new_relation<-
#' @exportMethod new_relation<-
setGeneric("new_relation<-", function(object, levels, value) {
  standardGeneric("new_relation<-")
})

#' @rdname new_relation
#' @aliases new_relation<-,vegtable,character,character-method
setReplaceMethod(
  "new_relation", signature(
    object = "vegtable",
    levels = "character",
    value = "character"
  ),
  function(object, levels, value) {
    return(new_relation(object, value, levels))
  }
)

#' @rdname new_relation
#' @aliases new_relation<-,vegtable,missing,character-method
setReplaceMethod(
  "new_relation", signature(
    object = "vegtable",
    levels = "missing",
    value = "character"
  ),
  function(object, value) {
    return(new_relation(object, value))
  }
)
