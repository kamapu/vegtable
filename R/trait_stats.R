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
#' @param FUN A function usually defined as `foo(x, ...)` or as
#'     `foo(x, w, ...)` for weighted statistics.
#' @param head_var Character value, the name of the variable at slot header to
#'     be used as aggregation level for the calculation of statistics or
#'     proportions.
#'     If not provided, the function will use **ReleveID** by default.
#' @param trait_level Character vector indicating a selection of levels from a
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
#' @param na.rm A logical value indicating whether NAs should be removed for the
#'     calculation of statistics or not. It is passed to `'FUN'` in
#'     `trait_stats()`.
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
#' ## Cocktail classification of plots
#' Wetlands_veg <- make_cocktail(Wetlands, Wetlands_veg, cover = "percen")
#'
#' ## Calculation of proportion of Cyperaceae species in the plot
#' Wetlands_veg <- trait_proportion("FAMILY", Wetlands_veg,
#'   trait_level = "Cyperaceae",
#'   weight = "percen", include_nas = FALSE
#' )
#'
#' ## Display of proportions per plant community
#' boxplot(Cyperaceae_prop ~ Syntax, Wetlands_veg@header, col = "grey")
#' @rdname trait_stats
NULL

#' @name check_args
#' @title Check validity of arguments for statistics
#' @description
#' Check validity of arguments in functions [trait_stats()] and
#' [trait_proportion()].
#' @keywords internal
check_args <- function(object, trait, head_var, weight, taxon_levels, merge_to) {
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
           weight, suffix = "_stats", in_header = TRUE, na.rm = TRUE, ...) {
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
        OUT[[i]] <- mapply(FUN, x = x, w = w, na.rm = na.rm, ...)
      }
      OUT <- as.data.frame(OUT)
    } else {
      OUT <- aggregate(
        as.formula(paste0(
          "cbind(", paste0(trait, collapse = ","), ") ~ ",
          head_var
        )), object@samples, FUN,
        na.rm = na.rm, ...
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
#'
#' @aliases trait_proportion,character,vegtable-method
#'
setMethod(
  "trait_proportion", signature(trait = "character", object = "vegtable"),
  function(trait, object, head_var, trait_level, taxon_levels, merge_to,
           include_nas = TRUE, weight, suffix = "_prop", in_header = TRUE,
           ...) {
    object_in <- object
    if (!missing(taxon_levels) | !missing(merge_to)) {
      object@species <- tax2traits(object@species, get_names = TRUE)
    }
    # Cross-check
    if (!all(trait %in% names(object@species@taxonTraits))) {
      trait <- trait[!trait %in% names(object@species@taxonTraits)]
      stop(paste0(
        "Following values in 'trait' are missing at ",
        "'object@species@taxonTraits': ", paste0(trait, collapse = ", "),
        "."
      ))
    }
    if (!missing(head_var)) {
      if (!head_var %in% colnames(object@header)) {
        stop("Value of argument 'head_var' is not a variable at header.")
      }
    }
    if (!missing(trait_level)) {
      if (any(!trait_level %in%
        paste(object@species@taxonTraits[, trait]))) {
        stop(paste(
          "Some values in argument 'trait_level'",
          "are not included in variable trait."
        ))
      }
    }
    if (!missing(taxon_levels)) {
      if (!taxon_levels %in% taxlist::levels(object@species)) {
        stop(paste(
          "Value of argument 'taxon_levels'",
          "is not included in the taxonomic list."
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
    # Transfer taxonomy to traits
    object@species@taxonTraits[, trait] <-
      paste(object@species@taxonTraits[, trait])
    object@species@taxonTraits[, trait] <-
      replace_x(
        object@species@taxonTraits[, trait], c(NA, ""),
        c("NA", "NA")
      )
    if (!include_nas) {
      object@species@taxonTraits[, trait] <-
        replace_x(
          object@species@taxonTraits[, trait], c("NA"),
          NA
        )
    }
    # Transfer traits to samples
    if (missing(merge_to)) {
      object <- taxa2samples(object, add_traits = TRUE)
    } else {
      object <- taxa2samples(object, merge_to, TRUE)
    }
    # Transfer head variable to samples
    if (!missing(head_var)) {
      object@samples[, head_var] <- with(
        object@header,
        replace_x(
          object@samples$ReleveID, ReleveID,
          get(head_var)
        )
      )
    } else {
      head_var <- "ReleveID"
    }
    object@samples <- object@samples[!is.na(object@samples[, trait]), ]
    # Aggregate to taxon
    object@samples$Level <- with(
      object@species@taxonRelations,
      paste(Level)[match(
        object@samples$TaxonConceptID,
        TaxonConceptID
      )]
    )
    if (!missing(taxon_levels)) {
      object@samples <- object@samples[object@samples$Level ==
        taxon_levels, ]
    }
    if (!missing(weight)) {
      object <- aggregate(
        as.formula(paste(
          weight, "~", head_var, "+",
          trait, "+ TaxonConceptID + Level"
        )),
        object@samples, sum
      )
      object <- crosstable(as.formula(paste(
        weight, "~", trait, "+",
        head_var
      )), object, sum,
      na_to_zero = TRUE
      )
      names(object)[-1] <- paste0(names(object)[-1], suffix)
    } else {
      object@samples$tax_count <- 1
      object <- aggregate(
        as.formula(paste(
          "tax_count ~", head_var,
          "+", trait,
          "+ TaxonConceptID + Level"
        )),
        object@samples, function(x) 1
      )
      object <- crosstable(as.formula(paste(
        "tax_count ~", trait, "+",
        head_var
      )), object, sum,
      na_to_zero = TRUE
      )
      names(object)[-1] <- paste0(names(object)[-1], suffix)
    }
    # Calculation of proportions
    object <- cbind(object[, 1], sweep(
      object[, -1], 1,
      rowSums(object[, -1]), "/"
    ))
    # Subset to requested levels
    if (!missing(trait_level)) {
      object <- object[, c(colnames(object)[1], paste0(
        trait_level,
        suffix
      ))]
    }
    # Finally the output
    names(object)[1] <- head_var
    # TODO: Here pass also to relations
    if (in_header) {
      if (head_var == "ReleveID") {
        for (i in colnames(object)[-1]) {
          object_in@header[, i] <- object[match(
            object_in@header[, head_var],
            object[, head_var]
          ), i]
        }
      } else {
        if (!head_var %in% names(object_in@relations)) {
          new_relation(object_in) <- head_var
        }
        for (i in colnames(object)[-1]) {
          object_in@relations[[head_var]][, i] <- object[match(
            object_in@relations[[head_var]][, head_var],
            object[, head_var]
          ), i]
        }
      }
      return(object_in)
    } else {
      return(object)
    }
  }
)

#' @rdname trait_stats
#'
#' @aliases trait_proportion,formula,vegtable-method
#'
setMethod(
  "trait_proportion", signature(trait = "formula", object = "vegtable"),
  function(trait, object, in_header = TRUE, ...) {
    head_var <- all.vars(update(trait, 0 ~ .))
    trait <- all.vars(update(trait, . ~ 0))
    OUT <- list()
    for (i in trait) {
      cat(paste0("Processing trait ", i, "...\n"))
      OUT[[i]] <- trait_proportion(i, object,
        head_var = head_var,
        in_header = FALSE, ...
      )
    }
    cat("DONE\n")
    if (in_header & head_var != "ReleveID") {
      warning(paste(
        "To insert in header 'ReleveID'",
        "is required as right term in the formula."
      ))
    }
    if (in_header & head_var == "ReleveID") {
      for (i in trait) {
        for (j in colnames(OUT[[i]])[-1]) {
          object@header[, j] <- with(
            OUT[[i]],
            get(j)[match(object@header$ReleveID, ReleveID)]
          )
        }
      }
    }
    if (!in_header | (in_header & head_var != "ReleveID")) {
      object <- data.frame(
        Var1 = unique(object@header[, head_var]),
        stringsAsFactors = FALSE
      )
      colnames(object) <- head_var
      for (i in trait) {
        for (j in colnames(OUT[[i]])[-1]) {
          object[, j] <- with(
            OUT[[i]],
            get(j)[match(object$ReleveID, get(head_var))]
          )
        }
      }
    }
    return(object)
  }
)
