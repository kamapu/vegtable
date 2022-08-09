#' @name braun_blanquet-data
#' @aliases braun_blanquet
#' @docType data
#'
#' @title Conversion of Braun-Blanquet codes to cover percentage
#'
#' @description
#' Cover values conversion as [coverconvert-class] object.
#'
#' Object of class [coverconvert-class] contains conversion tables
#' usually from a categorical variable (a cover scale) to a numerical one
#' (equivalent percentage cover value).
#' Cover values are stored as range for each level in the scale (minimum and
#' maximum cover value).
#'
#' @format An object of class \code{\linkS4class{coverconvert}}.
#'
#' @seealso [coverconvert-class] [cover_trans()]
#'
#' @examples
#' names(braun_blanquet)
#' summary(braun_blanquet)
#' summary(braun_blanquet$b_bbds)
"braun_blanquet"
