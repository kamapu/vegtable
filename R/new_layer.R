#' @name new_layer
#' @rdname new_layer
#'
#' @title Insert new classification of vegetation layers
#'
#' @description
#' A new information table for vegetation layers.
#'
#' @param object A [vegtable-class] object.
#' @param layer,value A data frame including information on vegetation layers.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' A [vegtable-class] object with the inserted new relation.
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
      stop("Duplicated values are not allowed in 'layer'.")
    }
    if (layer_name %in% names(object@layers)) {
      stop(paste0("layer '", layer, "' is already in 'object'."))
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
    return(new_layer(object, value, ...))
  }
)
