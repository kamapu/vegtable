#' @name layers2samples
#'
#' @title Add information from slot 'layers' into slot 'samples'
#'
#' @description
#' Slot layers may include additional information that should be moved to
#' samples in order to use it by [vegtable::subset()], [vegtable::aggregate()]
#' or [vegtable::crosstable()] methods.
#'
#' If names of variables are not provided, all variables from the respective
#' layer table will be inserted in slot `samples`.
#'
#' @param object An object of class [vegtable-class].
#' @param layer Character value indicating a target layer.
#' @param variable Character vector with the names of variables to be inserted
#'     in slot `samples`.
#' @param ... Further arguments to be passed among methods.
#' @return An object of class [vegtable-class] with variables added to samples.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#'
#' @rdname layers2samples
#'
#' @exportMethod layers2samples
#'
setGeneric(
  "layers2samples",
  function(object, layer, variable, ...) {
    standardGeneric("layers2samples")
  }
)

#' @rdname layers2samples
#'
#' @aliases layers2samples,vegtable,character,character-method
setMethod(
  "layers2samples", signature(
    object = "vegtable", layer = "character",
    variable = "character"
  ),
  function(object, layer, variable, ...) {
    if (!layer %in% names(object@layers)) {
      stop("Value of 'layer' is not occurring in slot 'layers'")
    }
    if (any(!variable %in% colnames(object@layers[[layer]]))) {
      stop(paste(
        "Some values of 'variable' are not occurring in",
        "the targeted layer table"
      ))
    }
    object@samples <- merge(object@samples, object@layers[[layer]][
      ,
      unique(c(layer, variable))
    ],
    by = layer, sort = FALSE,
    all.x = TRUE, all.y = FALSE
    )
    return(object)
  }
)

#' @rdname layers2samples
#'
#' @aliases layers2samples,vegtable,character,missing-method
setMethod(
  "layers2samples", signature(
    object = "vegtable", layer = "character",
    variable = "missing"
  ),
  function(object, layer, ...) {
    variable <- colnames(object@layers[[layer]])
    layers2samples(object, layer, variable)
  }
)
