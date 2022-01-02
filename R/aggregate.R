#' @name aggregate
#' @aliases aggregate,formula-method
#'
#' @title Aggregating information into a data frame
#'
#' @description
#' This function aggregates information contained in [vegtable-class] objects
#' to a summarizing data frame.
#'
#' This function works in a similar way as [crosstable()].
#'
#' @param x A formula indicating the variables used for the summary.
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
#' @seealso [stats::aggregate()]
#'
#' @exportMethod aggregate
#'
setMethod(
  "aggregate", signature(x = "formula"),
  function(x, data, FUN, use_nas = TRUE, ...) {
    if (class(data) != "vegtable") {
      return(stats::aggregate(x, data, FUN, ...))
    } else {
      Terms <- c(as.character(x)[2], attr(
        terms(x),
        "term.labels"
      ))
      data@samples <- data@samples[, colnames(data@samples) %in%
        c("ReleveID", "TaxonUsageID", Terms)]
      # Data from species
      data@samples$TaxonConceptID <- data@species@taxonNames[
        match(
          data@samples$TaxonUsageID,
          data@species@taxonNames$TaxonUsageID
        ),
        "TaxonConceptID"
      ]
      if ("TaxonName" %in% Terms & "AcceptedName" %in% Terms) {
        stop("Terms 'TaxonName' and 'AcceptedName' are mutually exclusive in 'formula'")
      }
      # 1: when usage name requested
      if ("TaxonName" %in% Terms) {
        data@samples$TaxonName <- data@species@taxonNames[
          match(
            data@samples$TaxonUsageID,
            data@species@taxonNames$TaxonUsageID
          ),
          "TaxonName"
        ]
        data@samples$AuthorName <- data@species@taxonNames[
          match(
            data@samples$TaxonUsageID,
            data@species@taxonNames$TaxonUsageID
          ),
          "AuthorName"
        ]
      }
      # 2: when accepted name requested
      if ("AcceptedName" %in% Terms) {
        data@samples$AcceptedName <- data@species@taxonRelations[
          match(
            data@samples$TaxonConceptID,
            data@species@taxonRelations$TaxonConceptID
          ),
          "AcceptedName"
        ]
        data@samples$AuthorName <- data@species@taxonNames[
          match(
            data@samples$AcceptedName,
            data@species@taxonNames$TaxonUsageID
          ),
          "AuthorName"
        ]
        data@samples$AcceptedName <- data@species@taxonNames[
          match(
            data@samples$AcceptedName,
            data@species@taxonNames$TaxonUsageID
          ),
          "TaxonName"
        ]
      }
      # Data from traits (only for Accepted Name or TaxonConceptID)
      traits_names <- colnames(data@species@taxonTraits)[
        colnames(data@species@taxonTraits) != "TaxonConceptID"
      ]
      if (any(Terms %in% traits_names)) {
        traits_names <- traits_names[traits_names %in% Terms]
        for (i in traits_names) {
          data@samples[, i] <- data@species@taxonTraits[
            match(
              data@samples$TaxonConceptID,
              data@species@taxonTraits$TaxonConceptID
            ), i
          ]
        }
      }
      # Data from header
      header_names <- colnames(data@header)[colnames(data@header) !=
        "ReleveID"]
      if (any(header_names %in% Terms)) {
        header_names <- header_names[header_names %in% Terms]
        for (i in header_names) {
          data@samples[, i] <- data@header[match(
            data@samples$ReleveID,
            data@header$ReleveID
          ), i]
        }
      }
      if (use_nas) {
        for (i in Terms[-1]) {
          if (is.factor(data@samples[, i])) {
            data@samples[, i] <- paste(data@samples[, i])
          }
          if (is.character(data@samples[, i])) {
            data@samples[is.na(data@samples[, i]), i] <- ""
          }
        }
      }
      return(stats::aggregate(x, data@samples, FUN, ...))
    }
  }
)
