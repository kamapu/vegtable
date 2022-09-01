#' @name relation2header
#'
#' @title Insert variables from relations into header
#'
#' @description
#' Information associated to categories listed in slot **relations** can be
#' inserted to slot **header** for further statistical comparisons.
#'
#' @param vegtable An [vegtable-class] object.
#' @param relation A character value indicating the relation to be used for
#'     inserting new variables in slot header.
#' @param vars A selection of variables from the relation to be inserted in
#'     header. This function will check the existence of the variables in the
#'     respective relation and retrieve an error if none is matching the names.
#'     If missing in the arguments, all variables of the respective relation
#'     will be inserted.
#' @param ... Further arguments passed among methods
#'
#' @return
#' A [vegtable-class] object.
#'
#' @author
#' Miguel Alvarez, <kamapu78@@gmail.com>
#'
#' @examples
#' ## Insert publication year of the source into header
#' veg <- relation2header(Kenya_veg, "REFERENCE", "YEAR")
#'
#' ## Show the frequency of plots per publication year
#' summary(as.factor(veg$YEAR))
#'
#' @rdname relation2heder
#' @export
relation2header <- function(vegtable, ...) {
  UseMethod("relation2header", vegtable)
}

#' @rdname relation2heder
#' @aliases relation2header,vegtable-method
#' @method relation2header vegtable
#' @export
relation2header.vegtable <- function(vegtable, relation, vars, ...) {
  if (!relation %in% names(vegtable@relations)) {
    stop(paste0(
      "The relation '", relation,
      "' is missing in 'vegtable@relations'."
    ))
  }
  if (missing(vars)) {
    vars <- names(vegtable@relations[[relation]])[
        names(vegtable@relations[[relation]]) != relation]
  } else {
    vars <- vars[vars != relation & vars %in%
            names(vegtable@relations[[relation]])]
  }
  if (length(vars) == 0) {
    stop("Values of 'vars' are not included in 'relation'.")
  }
  vegtable@header <- merge(vegtable@header, vegtable@relations[[relation]][
    ,
    c(relation, vars)
  ],
  by = relation, all.x = TRUE, sort = FALSE,
  suffixes = c("", "_y")
  )
  return(vegtable)
}
