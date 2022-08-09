#' @name veg_aggregate
#' @rdname veg_aggregate
#'
#' @title Aggregating information into a data frame
#'
#' @description
#' This function aggregates information contained in [vegtable-class] objects
#' to a summarizing data frame.
#'
#' This function works in a similar way as [crosstable()].
#'
#' @param object A formula indicating the variables used for the summary.
#' @param data Either a data frame or an object of class [vegtable-class].
#' @param FUN Function used to aggregate values.
#' @param use_nas Logical value indicating whether NA's should be included in
#'     categorical variables or not.
#' @param ... Further arguments passed to the function [stats::aggregate()].
#'
#' @return An object of class [data.frame].
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [aggregate()]
#'
#' @exportMethod veg_aggregate
setGeneric(
  "veg_aggregate",
  function(object, data, FUN, ...) {
    standardGeneric("veg_aggregate")
  }
)

#' @rdname veg_aggregate
#' @aliases veg_aggregate,formula,vegtable,function-method
setMethod(
  "veg_aggregate", signature(
    object = "formula", data = "vegtable",
    FUN = "function"
  ),
  function(object, data, FUN, use_nas = TRUE, ...) {
    Terms <- c(as.character(object)[2], attr(
      terms(object),
      "term.labels"
    ))
    if (any(Terms %in% names(data@species@taxonTraits))) {
      data <- taxa2samples(data, add_traits = TRUE)
    }
    if (any(c("TaxonName", "AcceptedName") %in% Terms)) {
      if (all(c("TaxonName", "AcceptedName") %in% Terms)) {
        stop(paste(
          "Terms 'TaxonName' and 'AcceptedName'",
          "are mutually exclusive in 'formula'"
        ))
      }
      data <- taxa2samples(data)
    }
    # Variables from samples
    if (any(Terms %in% names(data@samples))) {
      new_data <- data@samples[, colnames(data@samples) %in%
        c("ReleveID", "TaxonUsageID", "TaxonConceptID", Terms), drop = FALSE]
    } else {
      if (any(Terms %in% c("TaxonName", "AcceptedName"))) {
        new_data <- data@samples[
          ,
          c("ReleveID", "TaxonUsageID", "TaxonConceptID")
        ]
      } else {
        new_data <- data.frame(ReleveID = integer())
      }
    }
    # 1: when usage name requested
    if ("TaxonName" %in% Terms) {
      new_data$TaxonName <- data@species@taxonNames$TaxonName[
        match(new_data$TaxonUsageID, data@species@taxonNames$TaxonUsageID)
      ]
      new_data$AuthorName <- data@species@taxonNames$AuthorName[
        match(new_data$TaxonUsageID, data@species@taxonNames$TaxonUsageID)
      ]
    }
    # 2: when accepted name requested
    if ("AcceptedName" %in% Terms) {
      new_data$AcceptedNameID <- data@species@taxonRelations$AcceptedName[
        match(
          new_data$TaxonConceptID,
          data@species@taxonRelations$TaxonConceptID
        )
      ]
      new_data$AcceptedName <- data@species@taxonNames$TaxonName[
        match(new_data$AcceptedNameID, data@species@taxonNames$TaxonUsageID)
      ]
      new_data$AuthorName <- data@species@taxonNames$AuthorName[
        match(new_data$AcceptedNameID, data@species@taxonNames$TaxonUsageID)
      ]
    }
    # Data from header
    if (any(Terms %in% names(data@header))) {
      new_data <- merge(new_data, data@header[, names(data@header) %in%
        c("ReleveID", Terms), drop = FALSE], sort = FALSE, all.y = TRUE)
    }
    # Call aggregate on new_data
    if (use_nas) {
      for (i in Terms[-1]) {
        if (is.factor(new_data[, i])) {
          new_data[, i] <- paste(new_data[, i])
        }
        if (is.character(new_data[, i])) {
          new_data[is.na(new_data[, i]), i] <- ""
        }
      }
    }
    return(aggregate(object, new_data, FUN, ...))
  }
)
