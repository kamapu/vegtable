#' @name veg_relation
#'
#' @title Retrieve or replace relations in vegtable objects
#'
#' @description
#' Tables providing information about levels of categorical variables in the
#' header are called `popups` in **Turboveg** databases
#' but `relations` in [vegtable-class] objects.
#' Such variables will be converted into factors in the slot `header` according
#' to the levels and their sorting in the respective relation.
#'
#' @param vegtable An object of class [vegtable-class].
#' @param relation A character value indicating the relation table to be
#'     retrieved or replaced.
#' @param match_header A logical vector, whether only levels occurring in slot
#'     `header` should be considered or all.
#' @param value A data frame containing the new veg_relation.
#' @param ... Further arguments to be passed among methods.
#'
#' @return
#' This function retrieves and object of class `data.frame`.
#' In the replacement method, an object of class [vegtable-class], including
#' `value` in the slot `relations`.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## overview of references
#' veg_relation(Kenya_veg, "REFERENCE")
#'
#' @rdname veg_relation
#'
#' @exportMethod veg_relation
#'
setGeneric(
  "veg_relation",
  function(vegtable, relation, ...) {
    standardGeneric("veg_relation")
  }
)

#' @rdname veg_relation
#'
#' @aliases veg_relation,vegtable,character-method
#'
setMethod(
  "veg_relation", signature(vegtable = "vegtable", relation = "character"),
  function(vegtable, relation, match_header = FALSE, ...) {
    if (match_header) {
      return(vegtable@relations[[relation]][
        vegtable@relations[[relation]][, relation] %in%
          vegtable@header[, relation],
      ])
    } else {
      return(vegtable@relations[[relation]])
    }
  }
)

#' @rdname veg_relation
#' @aliases veg_relation<-
#' @exportMethod veg_relation<-
setGeneric("veg_relation<-", function(vegtable, value) {
  standardGeneric("veg_relation<-")
})

#' @rdname veg_relation
#' @aliases veg_relation<-,vegtable,data.frame-method
setReplaceMethod(
  "veg_relation", signature(
    vegtable = "vegtable",
    value = "data.frame"
  ),
  function(vegtable, value) {
    relation <- names(value)[1]
    if (is(value[, relation], "factor")) {
      levels <- base::levels(value[, relation])
    } else {
      levels <- as.character(unique(value[, relation]))
      value[, relation] <- factor(as.character(value[, relation]),
        levels = levels
      )
    }
    if (relation %in% names(vegtable@header)) {
      if (is(vegtable@header[, relation], "factor")) {
        levels_in <- base::levels(vegtable@header[, relation])
      } else {
        levels_in <- as.character(unique(vegtable@header[
          !is.na(vegtable@header[, relation]), relation
        ]))
      }
      if (!all(levels_in %in% levels)) {
        missing_levels <- levels_in[!levels_in %in% levels]
        stop(paste0(
          "Following values are missing in the input relation: ",
          paste0(missing_levels, collapse = ", ")
        ))
      }
    }
    vegtable <- new_relation(vegtable, relation, levels)
    vegtable@relations[[relation]] <- value
    return(vegtable)
  }
)
