#' @name new_layer
#' @rdname new_layer
#'
#' @title Insert new classification of vegetation layers
#'
#' @description
#' A new information table for vegetation layers.
#'
#' @param object A [vegtable-class] object.
#' @param layer A data frame including information on vegetation layers.
#' @param value Either a data frame or a character value. In the second case,
#'     this value indicates the name of the variable at slot samples that will
#'     be set as layer information.
#' @param levels A character vector used to set the levels of the new layer.
#'     This is only used in the replacement method using a character value.
#'     This input is mandatory when the new layer does not exist in slot
#'     samples, otherwise an error message will be retrieved.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' A [vegtable-class] object with the inserted new relation.
#'
#' @seealso
#' [layers2samples()]
#'
#' @example examples/new_layer.R
#'
#' @rdname new_layer
#' @exportMethod new_layer
setGeneric(
  "new_layer",
  function(object, layer, ...) {
    standardGeneric("new_layer")
  }
)

#' @rdname new_layer
#' @aliases new_layer,vegtable,data.frame-method
setMethod(
  "new_layer", signature(
    object = "vegtable", layer = "data.frame"
  ),
  function(object, layer, ...) {
    # Conditions
    layer_name <- names(layer)[1]
    if (any(duplicated(layer[[layer_name]]))) {
      stop(paste0("Duplicated values are not allowed for layer '", layer_name,
              "'."))
    }
    if (layer_name %in% names(object@layers)) {
      stop(paste0("layer '", layer_name, "' is already in 'object'."))
    }
    # Add empty layer if not existing in samples
    if (!layer_name %in% names(object@samples)) {
      object@samples[, names(layer)[1]] <- NA
    }
    # Factorize in layer
    if (!is(layer[[layer_name]], "factor")) {
      layer[[layer_name]] <- as.factor(as.character(layer[[layer_name]]))
    }
    # Factorize in samples
    object@samples[[layer_name]] <-
      factor(as.character(object@samples[[layer_name]]),
        levels = levels(layer[[layer_name]])
      )
    # Insert layer
    object@layers[[layer_name]] <- layer
    return(object)
  }
)

#' @rdname new_layer
#' @aliases new_layer<-
#' @exportMethod new_layer<-
setGeneric("new_layer<-", function(object, ..., value) {
  standardGeneric("new_layer<-")
})

#' @rdname new_layer
#' @aliases new_layer<-,vegtable,data.frame-method
setReplaceMethod(
  "new_layer", signature(
    object = "vegtable",
    value = "data.frame"
  ),
  function(object, ..., value) {
    return(new_layer(object, value))
  }
)

#' @rdname new_layer
#' @aliases new_layer<-,vegtable,character-method
setReplaceMethod(
  "new_layer", signature(
    object = "vegtable",
    value = "character"
  ),
  function(object, levels, ..., value) {
    if (!value %in% names(object@samples) & missing(levels)) {
      stop(paste0(
        "Missing variable '", value, "' in slot samples. ",
        "You need to specify 'levels' for this method."
      ))
    }
    if (value %in% names(object@samples) & missing(levels)) {
      if (!is(object@samples[[value]], "factor")) {
        object@samples[[value]] <-
          as.factor(as.character(object@samples[[value]]))
      }
      levels <- base::levels(object@samples[[value]])
    }
    value_out <- list()
    value_out[[value]] <- factor(as.character(levels),
      levels = as.character(levels)
    )
    return(new_layer(object, as.data.frame(value_out)))
  }
)
