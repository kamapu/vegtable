#' @name trait_stats
#'
#' @title Statistics and proportion for taxon traits
#'
#' @description
#' Calculation of statistics and proportions of taxon traits for plot
#' observations or groups of observations, considering data relationships,
#' taxonomic ranks and the handling of not available values.
#'
#' In `trait_stats()` you can use customized functions, which have to be
#' defined as `foo(x, w, ...)`, where `'x'` is the (numeric) taxon trait and
#' `'w'` is the weight (e.g. the abundance).
#'
#' With the arguments `taxon_levels` and `merge_to` the used taxonomic ranks
#' can be defined, where the first one indicates which ranks
#' have to be considered in the calculations and the second one determine the
#' aggregation of taxa from a lower level to a parental one.
#'
#' @param trait Either a character value indicating the name of trait variable
#'     or a formula as `'trait ~ head_var'`. Note that you can add multiple
#'     variables in the form `trait_1 + ... + trait_n ~ head_var`.
#' @param object A [vegtable-class] object.
#' @param FUN A function usually defined as `foo(x)` or as
#'     `foo(x, w)` for weighted statistics.
#' @param head_var Character value, the name of the variable at slot header to
#'     be used as aggregation level for the calculation of statistics or
#'     proportions.
#'     If not provided, the function will use **ReleveID** by default.
#' @param trait_levels Character vector indicating a selection of levels from a
#'     trait, in the case that some levels should be ignored in the output.
#'     Trait levels that are skipped at output will be still used for the
#'     calculation of proportions.
#'     This argument gets only applied for the character method.
#' @param taxon_levels Character vector indicating the selected taxonomic ranks
#'     to be considered in the output.
#' @param merge_to Character value indicating the taxonomic rank for
#'     aggregation of taxa.
#'     All ranks lower than the one indicated here will be assigned to the
#'     respective parents at the required taxonomic rank.
#' @param include_nas Logical value indicating whether NAs should be considered
#'     for the calculation of proportions or not.
#' @param weight Character value indicating the name of the variable at slot
#'     **samples** used as weight for the proportions. Usually the numeric
#'     abundance.
#' @param suffix A suffix added to the name of the trait variable or to the
#'     levels of categorical trait variables. I is meant to avoid homonymous
#'     variables within the same object.
#' @param in_header Logical value indicating whether the output should be
#'     inserted in the slot **header** or provided as data frame. In the case
#'     that `'head_var'` (or the right term in the formula method) is different
#'     from **ReleveID**, the statistics and proportions will be inserted in the
#'     respective data frame at slot **relations**.
#' @param ... Further arguments passed among methods. In the case of the
#'     character method, they are passed to 'FUN'.
#'
#' @return
#' A data frame with the proportions of traits levels or statistics for
#' the trait variable, or an object of class [vegtable-class] including those
#' results at the slot `header`.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' veg <- cover_trans(Kenya_veg, to = "cover")
#' veg <- trait_proportion("lf_behn_2018", veg,
#'   trait_levels = "obligate_annual", weight = "cover", include_nas = FALSE
#' )
#' summary(veg$obligate_annual_prop)
#' @rdname trait_stats
NULL

#' @name check_args
#' @title Check validity of arguments for statistics
#' @description
#' Check validity of arguments in functions [trait_stats()] and
#' [trait_proportion()].
#' @keywords internal
check_args <- function(object, trait, head_var, weight, taxon_levels, merge_to,
                       trait_levels) {
  if (!all(trait %in% names(object@species@taxonTraits))) {
    trait <- trait[!trait %in% names(object@species@taxonTraits)]
    stop(paste0(
      "Following values in 'trait' are missing at ",
      "'object@species@taxonTraits': ",
      paste0(trait, collapse = ", "), "."
    ))
  }
  if (!head_var %in% colnames(object@header)) {
    stop("Value of argument 'head_var' is not a variable at header.")
  }
  if (!missing(taxon_levels)) {
    if (!all(taxon_levels %in% taxlist::levels(object@species))) {
      taxon_levels <- taxon_levels[!taxon_levels %in%
        taxlist::levels(object@species)]
      stop(paste0(
        "Following values in 'taxon_levels'",
        "are not taxonomic ranks at 'object@species': ",
        paste0(taxon_levels, collapse = ", "), "."
      ))
    }
  }
  if (!missing(merge_to)) {
    if (!merge_to %in% taxlist::levels(object@species)) {
      stop(paste(
        "Value of in argument 'merge_to'",
        "is not included in the taxonomic list."
      ))
    }
  }
  if (!missing(weight)) {
    if (!weight %in% colnames(object@samples)) {
      stop("Value of argument 'weight' is not included at slot samples.")
    }
  }
  if (!missing(trait_levels)) {
    if (!all(trait_levels %in% object@species@taxonTraits[, trait])) {
      trait_levels <- trait_levels[!trait_levels %in%
        object@species@taxonTraits[, trait]]
      stop(paste0(
        "Following values of 'trait_levels' ",
        "are not included in trait '", trait, "': '",
        paste0(trait_levels, collapse = "', '"), "'."
      ))
    }
  }
}

#' @rdname trait_stats
#' @exportMethod trait_stats
setGeneric(
  "trait_stats",
  function(trait, object, ...) {
    standardGeneric("trait_stats")
  }
)

#' @rdname trait_stats
#' @aliases trait_stats,character,vegtable-method
setMethod(
  "trait_stats", signature(trait = "character", object = "vegtable"),
  function(trait, object, FUN, head_var = "ReleveID", taxon_levels, merge_to,
           weight, suffix = "_stats", in_header = TRUE, ...) {
    # Check conditions
    check_args(object, trait, head_var, weight, taxon_levels, merge_to)
    # Retain original object
    object_in <- object
    # Transfer head variable to samples
    if (head_var != "ReleveID") {
      object@samples[, head_var] <- with(
        object@header,
        replace_x(
          object@samples$ReleveID, ReleveID,
          get(head_var)
        )
      )
    }
    # Get traits in slot samples
    if (missing(taxon_levels)) {
      taxon_levels <- taxlist::levels(object_in@species)
    }
    if (missing(merge_to)) {
      object <- taxa2samples(object,
        include_levels = taxon_levels,
        add_traits = TRUE
      )
    } else {
      object <- taxa2samples(object,
        include_levels = taxon_levels,
        add_traits = TRUE, merge_to = merge_to
      )
    }
    # In case of weighted statistics
    if (!missing(weight)) {
      OUT <- list()
      OUT[[head_var]] <- unique(object@samples[, head_var])
      for (i in trait) {
        x <- with(object@samples, split(get(i), get(head_var)))
        w <- with(object@samples, split(get(weight), get(head_var)))
        OUT[[i]] <- mapply(FUN, x = x, w = w, ...)
      }
      OUT <- as.data.frame(OUT)
    } else {
      OUT <- aggregate(
        as.formula(paste0(
          "cbind(", paste0(trait, collapse = ","), ") ~ ",
          head_var
        )), object@samples, FUN, ...
      )
    }
    names(OUT) <- replace_x(names(OUT),
      old = trait,
      new = paste0(trait, suffix)
    )
    # Finally the output
    if (in_header) {
      if (head_var == "ReleveID") {
        for (i in names(OUT)[names(OUT) != head_var]) {
          object_in@header[, i] <- OUT[match(
            object_in@header[, head_var],
            OUT[, head_var]
          ), i]
        }
        return(object_in)
      } else {
        if (!head_var %in% names(object_in@relations)) {
          new_relation(object_in) <- head_var
        }
        for (i in names(OUT)[names(OUT) != head_var]) {
          object_in@relations[[head_var]][, i] <- OUT[
            match(
              object_in@relations[[head_var]][, head_var],
              OUT[, head_var]
            ), i
          ]
        }
        return(object_in)
      }
    } else {
      return(OUT)
    }
  }
)

#' @rdname trait_stats
#' @aliases trait_stats,formula,vegtable-method
setMethod(
  "trait_stats", signature(trait = "formula", object = "vegtable"),
  function(trait, object, ...) {
    head_var <- all.vars(update(trait, 0 ~ .))
    trait <- all.vars(update(trait, . ~ 0))
    return(trait_stats(trait, object, head_var = head_var, ...))
  }
)

#' @rdname trait_stats
#' @aliases trait_proportion
#' @exportMethod trait_proportion
setGeneric(
  "trait_proportion",
  function(trait, object, ...) {
    standardGeneric("trait_proportion")
  }
)

#' @rdname trait_stats
#' @aliases trait_proportion,character,vegtable-method
setMethod(
  "trait_proportion", signature(trait = "character", object = "vegtable"),
  function(trait, object, head_var = "ReleveID", trait_levels, taxon_levels,
           merge_to, include_nas = TRUE, weight, suffix = "_prop", in_header = TRUE,
           ...) {
    # Only one trait is allowed for this function
    if (length(trait) > 1) {
      stop("Only one trait can be assessed by 'trait_proportion'.")
    }
    # Check conditions
    check_args(
      object, trait, head_var, weight, taxon_levels, merge_to,
      trait_levels
    )
    # Retain original object
    object_in <- object
    # Transfer head variable to samples
    if (head_var != "ReleveID") {
      object@samples[, head_var] <- with(
        object@header,
        replace_x(
          object@samples$ReleveID, ReleveID,
          get(head_var)
        )
      )
    }
    # Get traits in slot samples
    if (missing(taxon_levels)) {
      taxon_levels <- taxlist::levels(object_in@species)
    }
    if (missing(merge_to)) {
      object <- taxa2samples(object,
        include_levels = taxon_levels,
        add_traits = TRUE
      )
    } else {
      object <- taxa2samples(object,
        include_levels = taxon_levels,
        add_traits = TRUE, merge_to = merge_to
      )
    }
    # Include NAs
    if (include_nas) {
      object@samples[is.na(object@samples[, trait]), trait] <- "NA"
    } else {
      object@samples <- object@samples[!is.na(object@samples[, trait]), ]
    }
    # Calculate output
    if (missing(weight)) {
      weight <- "count_presence"
      count_presence <- rep(1, nrow(object@samples))
    }
    OUT <- aggregate(as.formula(paste(weight, "~", trait, "+", head_var)),
      data = object@samples, FUN = sum
    )
    SUM <- aggregate(as.formula(paste(weight, "~", head_var)),
      data = object@samples, FUN = sum
    )
    SUM <- SUM[match(OUT[, head_var], SUM[, head_var]), weight]
    OUT[, weight] <- OUT[, weight] / SUM
    # Filter to selected trait levels
    if (!missing(trait_levels)) {
      OUT <- OUT[OUT[, trait] %in% trait_levels, ]
    }
    OUT[, trait] <- paste0(OUT[, trait], suffix)
    OUT <- crosstable(
      as.formula(paste(
        weight, "~", trait, "+",
        head_var
      )), OUT, sum,
      na_to_zero = TRUE
    )
    # Last output
    if (in_header) {
      if (head_var == "ReleveID") {
        for (i in names(OUT)[names(OUT) != head_var]) {
          object_in@header[, i] <- OUT[match(
            object_in@header[, head_var],
            OUT[, head_var]
          ), i]
        }
        return(object_in)
      } else {
        if (!head_var %in% names(object_in@relations)) {
          new_relation(object_in) <- head_var
        }
        for (i in names(OUT)[names(OUT) != head_var]) {
          object_in@relations[[head_var]][, i] <- OUT[
            match(
              object_in@relations[[head_var]][, head_var],
              OUT[, head_var]
            ), i
          ]
        }
        return(object_in)
      }
    } else {
      return(OUT)
    }
  }
)

#' @rdname trait_stats
#' @aliases trait_proportion,formula,vegtable-method
setMethod(
  "trait_proportion", signature(trait = "formula", object = "vegtable"),
  function(trait, object, ...) {
    head_var <- all.vars(update(trait, 0 ~ .))
    trait <- all.vars(update(trait, . ~ 0))
    return(trait_proportion(trait, object, head_var = head_var, ...))
  }
)
