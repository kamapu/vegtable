#' @name transform
#'
#' @title Convert cover scales to percent cover
#'
#' @description
#' Convert values of a categorical cover scale to percentage values.
#'
#' This function requires as input a [coverconvert-class] object which contains
#' the conversion tables.
#'
#' In the case of [vegtable-class] objects, the conversion is already embedded
#' in the slot `coverconvert`.
#'
#' Three rules are implemented for transformation, either `top` (values
#' transformed to the top of the range), `middle` (transformation at the
#' midpoint), and `bottom` (conversion at the lowest value of the
#' range). In the later case, transformation ranges starting at 0% of cover
#' can be set to a different value by the argument `zeroto`.
#'
#' When `replace=FALSE`, existing values of cover in the [vegtable-class]
#' object will be maintained. Since there is not a standard naming of cover
#' values, in the transformation the name of cover variable should be
#' indicated in the argument `to`.
#'
#' @param _data,x Either a factor or character vector, or a [vegtable-class] object.
#' @param conversion An object of class [vegtable-class].
#' @param from Scale name of values in `x` as character value.
#' @param to Name of the column in slot `samples` for writing converted values.
#' @param replace Logical value indicating whether existing cover values should
#'     be replaced or not.
#' @param rule Rule applied for the conversion (see details).
#' @param zeroto Value used to replace levels with bottom at 0% cover.
#' @param ... Further arguments passed from or to other methods.
#'
#' @return Either a vector or a [vegtable-class] object.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [vegtable-deprecated]
#'
#' @rdname transform
#'
#' @keywords internal
#'
NULL

#' @rdname transform
#'
#' @export
#'
transform <- function(x, ...) {
  .Deprecated(new = "cover_trans")
}
