#' @name make_cocktail
#' 
#' @title Produce a Cocktail classification
#' 
#' @description 
#' Classification of [vegtable-class] objects according to **Cocktail**
#' algorithms.
#' 
#' Cocktail algorithms are logical functions selecting plots according to
#' either occurrence of species groups and cover values of single species. A
#' group will be declared as occurring in a plot when at least a half of its
#' members is present in the plot.
#' 
#' This function inserts single columns with logical values indicating whether
#' a plot is classified in the vegetation unit or not. An additional column
#' (name provided in argument `syntax`) compile all vegetation units,
#' indicating with a `+` symbol those plots classified in more than one
#' vegetation unit.
#' When only a part of the formulas will be used, it should be
#' specified by the argument `which`.
#' 
#' These functions are implemented for constructing or complementing
#' [shaker-class] objects.
#' Note that construction of those objects will always require a `companion`
#' object, which is either an object of class [taxlist-class] or
#' [vegtable-class].
#' 
#' @param shaker An object of class [shaker-class] containing the respective
#'     cocktail definitions.
#' @param vegtable An object of class [vegtable-class] containing the
#'     vegetation observations to be classified.
#' @param which Integer or character indicating the definition to be applied
#'     for classification.
#' @param cover Name of the cover variable in `vegtable`.
#' @param syntax Character value indicating the name of the retrieved variable
#'     including the final classification of plots.
#' @param FUN Function used for merging multiple occurrence of species in a
#'     single plot.
#' @param companion Either a [taxlist-class] or a [vegtable-class] object.
#' @param pseudo,group Character vector with names of taxa included in a
#'     pseudo-species or a species group.
#' @param formula Character vector including a formula as definition of a
#'     vegetation unit.
#' @param pseudo_id,group_id,formula_id Character value as name of the
#'     pseudo-species, species group or defined vegetation unit.
#' @param authority Logical value indicating whether author names should be
#'     included in the taxon name or not.
#' @param enc_cont,enc_gr Encodings used for special characters.
#' @param ... Further arguments passes from or to other methods.
#' 
#' @return
#' A data frame corresponding to the slot `header` of input object `vegtable`,
#' including the results of Cocktail classification for the respective plots.
#' 
#' A [shaker-class] object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [shaker-class] [vegtable-class] [Wetlands]
#' 
#' @references
#' \bold{Alvarez M (2017).} Classification of aquatic and
#' semi-aquatic vegetation in two East African sites: Cocktail definitions and
#' syntaxonomy. \emph{Phytocoenologia}.
#' 
#' \bold{Bruelheide H (2000).} A new measure of fidelity and its application to
#' defining species groups. \emph{Journal of Vegetation Science} 11: 167--178.
#' 
#' \bold{Kočí M, Chytrý M, Tichý L (2003).} Formalized reproduction of an
#' expert-based phytosociological classification: a case study of subalpine
#' tall-forb vegetation. \emph{Journal of Vegetation Science} 14: 601--610.
#' 
#' @examples
#' ## Example from Alvarez (2017)
#' Wetlands_veg@@header <- make_cocktail(Wetlands, Wetlands_veg, cover="percen")
#' summary(as.factor(Wetlands_veg@@header$Syntax))
#' 
#' ## Same but only for two vegetation units
#' Wetlands_veg@@header <- make_cocktail(Wetlands, Wetlands_veg,
#'     which=c("HY1","HY2"), cover="percen")
#' summary(as.factor(Wetlands_veg$Syntax))
#' 
#' ## Construct the 'shaker' object anew
#' Wetlands <- new("shaker")
#' 
#' ## Set a pseudo-species
#' Wetlands <- set_pseudo(Wetlands, Wetlands_veg, c("Cyperus latifolius",
#' 				"Cyperus exaltatus"))
#' 
#' ## Set a species group
#' Wetlands <- set_group(Wetlands, Wetlands_veg, group_id="Cyperus papyrus",
#' 		group=c(
#'                 "Cyperus papyrus",
#'                 "Cyclosorus interruptus",
#'                 "Lepistemon owariense"))
#' 
#' ## Set a fromula
#' Wetlands <- set_formula(Wetlands, Wetlands_veg, formula_id="HE1",
#' 		formula="groups:'Cyperus papyrus' | species:'Cyperus papyrus > 50'")
#' 
#' ## Summaries
#' summary(Wetlands)
#' summary(Wetlands, Wetlands_veg)
#' 
#' @rdname make_cocktail
#' 
#' @aliases set_group
#' 
#' @exportMethod set_group
#' 
setGeneric("set_group",
		function(shaker, companion, group, ...)
			standardGeneric("set_group")
)

#' @rdname make_cocktail
#' 
#' @aliases set_group,shaker,taxlist,character-method
#' 
setMethod("set_group", signature(shaker="shaker", companion="taxlist",
				group="character"),
		function(shaker, companion, group, group_id, authority=FALSE,
				enc_cont="latin1", enc_gr="utf8", ...) {
			companion <- accepted_name(companion)
			if(authority) {
				companion$AuthorName[is.na(companion$AuthorName)] <- ""
				companion$TaxonName <- with(companion, paste(TaxonName,
								AuthorName))
			}
			companion$TaxonName <- iconv(companion$TaxonName, enc_cont,
					"ASCII//TRANSLIT")
			group <- iconv(group, enc_gr, "ASCII//TRANSLIT")
			if(any(!group %in% companion$TaxonName))
				stop("Some names in 'group' are not in 'companion'")
			if(any(duplicated(group)))
				group <- unique(group)
			if(any(duplicated(companion$TaxonName)))
				warning(paste("Some duplicated names in 'companion', only",
								"one will be retrieved"))
			if(missing(group_id))
				group_id <- length(shaker@groups) + 1
			shaker@groups[[group_id]] <- companion[charmatch(group,
							companion$TaxonName),"TaxonConceptID"]
			return(shaker)
		}
)

#' @rdname make_cocktail
#' 
#' @aliases set_group,shaker,vegtable,character-method
#' 
setMethod("set_group", signature(shaker="shaker", companion="vegtable",
				group="character"),
		function(shaker, companion, group, ...) {
			set_group(shaker, companion@species, group, ...)
		}
)

#' @rdname make_cocktail
#' 
#' @aliases set_pseudo
#' 
#' @exportMethod set_pseudo
#' 
setGeneric("set_pseudo",
		function(shaker, companion, pseudo, ...)
			standardGeneric("set_pseudo")
)

#' @rdname make_cocktail
#' 
#' @aliases set_pseudo,shaker,taxlist,character-method
#' 
setMethod("set_pseudo", signature(shaker="shaker", companion="taxlist",
				pseudo="character"),
		function(shaker, companion, pseudo, pseudo_id, authority=FALSE,
				enc_cont="latin1", enc_gr="utf8", ...) {
			companion <- accepted_name(companion)
			if(authority) {
				companion$AuthorName[is.na(companion$AuthorName)] <- ""
				companion$TaxonName <- with(companion, paste(TaxonName,
								AuthorName))
			}
			companion$TaxonName <- iconv(companion$TaxonName, enc_cont,
					"ASCII//TRANSLIT")
			pseudo <- iconv(pseudo, enc_gr, "ASCII//TRANSLIT")
			if(any(!pseudo %in% companion$TaxonName))
				stop("Some names in 'pseudo' are not in 'companion'")
			if(any(duplicated(companion$TaxonName)))
				warning(paste("Some duplicated names in 'companion', only",
								"one will be retrieved"))
			if(missing(pseudo_id))
				pseudo_id <- length(shaker@pseudos) + 1
			shaker@pseudos[[pseudo_id]] <- companion[charmatch(pseudo,
							companion$TaxonName),"TaxonConceptID"]
			return(shaker)
		}
)

#' @rdname make_cocktail
#' 
#' @aliases set_pseudo,shaker,vegtable,character-method
#' 
setMethod("set_pseudo", signature(shaker="shaker", companion="vegtable",
				pseudo="character"),
		function(shaker, companion, pseudo, ...) {
			set_pseudo(shaker, companion@species, pseudo, ...)
		}
)

#' @rdname make_cocktail
#' 
#' @aliases set_formula
#' 
#' @exportMethod set_formula
#' 
setGeneric("set_formula",
		function(shaker, companion, formula, ...)
			standardGeneric("set_formula")
)

#' Have to be described
#' 
#' @keywords internal
#' 
format_F2 <- function(x) {
	x <- paste(x, collapse=" ")
	x <- gsub(" ", "", x, fixed=TRUE)
	x
}

#' @rdname make_cocktail
#' 
#' @aliases set_formula,shaker,taxlist,character-method
#' 
setMethod("set_formula", signature(shaker="shaker", companion="taxlist",
				formula="character"),
		function(shaker, companion, formula, formula_id, authority=FALSE,
				enc_cont="latin1", enc_gr="utf8", ...) {
			if(grepl("\'", formula)) SYM <- "\'"
			if(grepl('\"', formula)) SYM <- '\"'
			Names <- Names_old <- rm_between(formula, left=SYM, right=SYM,
					extract=TRUE)[[1]]
			Slots <- strsplit(formula, " ")[[1]]
			Slots[grepl("species:", Slots)] <- "species"
			Slots[grepl("groups:", Slots)] <- "groups"
			Slots <- Slots[Slots %in% c("species","groups")]
			# Check existence of groups in shaker object
			
			if(any(!Names[Slots == "groups"] %in% names(shaker@groups)))
				stop(paste("Some groups mentioned in the 'formula' are not",
								"included in 'shaker'"))
			# Retrieve
			companion <- accepted_name(companion)
			if(authority) {
				companion$AuthorName[is.na(companion$AuthorName)] <- ""
				companion$TaxonName <- with(companion, paste(TaxonName,
								AuthorName))
			}
			if(any(Slots == "species")) {
				dominants <- strsplit(Names[Slots == "species"], " ")
				# function to merge species names elements
				dominants <- as.data.frame(do.call(rbind, lapply(dominants,
										function(x) {
											NR <- length(x)
											return(c(paste(x[1:(NR - 2)],
																collapse=" "),
														x[(NR - 1):NR]))
										})), stringsAsFactors=FALSE)
				colnames(dominants) <- c("TaxonConceptID", "operator", "value")
				# In case of use of authority
				companion$TaxonName <- iconv(companion$TaxonName, enc_cont,
						"ASCII//TRANSLIT")
				dominants$TaxonConceptID <- iconv(dominants$TaxonConceptID,
						enc_gr, "ASCII//TRANSLIT")
				if(any(!dominants$TaxonConceptID %in% companion$TaxonName))
					stop(paste("Some species in 'formula' are not included",
									"in 'companion'"))
				dominants$TaxonConceptID <- companion[
						match(dominants$TaxonConceptID, companion$TaxonName),
						"TaxonConceptID"]
				dominants$value <- as.numeric(dominants$value)
				# paste rows in Names before continuing
				Names[Slots == "species"] <- formula_new <- apply(dominants, 1,
						format_F2)
				# merge slot dominants and extract duplicated
				if(nrow(shaker@dominants) > 0) {
					formula_old <- apply(shaker@dominants, 1, format_F2)
					dominants <- do.call(rbind, list(shaker@dominants,
									dominants[!formula_new %in% formula_old,]))
					rownames(dominants) <- NULL
					formula_new <- apply(dominants, 1, format_F2)
				}
				shaker@dominants <- dominants
				# reformat formula
				Names[Slots == "species"] <- paste(match(Names[Slots ==
												"species"], formula_new))
			}
			# reformat formula (continuation)
			Names[Slots == "groups"] <- paste0(SYM, Names[Slots == "groups"],
					SYM)
			# Assemble new formulas
			Names_old <- paste0(Slots, ":", SYM, Names_old, SYM)
			Slots[Slots == "species"] <- "dominants"
			Names <- paste0(Slots, "[[", Names, "]]")
			for(i in 1:length(Names))
				formula <- sub(Names_old[i], Names[i], formula)
			# Insert results in output object
			
			if(missing(formula_id))
				formula_id <- length(shaker@formulas) + 1
			shaker@formulas[[formula_id]] <- formula
			return(shaker)
		}
)

#' @rdname make_cocktail
#' 
#' @aliases set_formula,shaker,vegtable,character-method
#' 
setMethod("set_formula", signature(shaker="shaker", companion="vegtable",
				formula="character"),
		function(shaker, companion, formula, ...) {
			set_formula(shaker, companion@species, formula, ...)
		}
)

#' @rdname make_cocktail
#' 
#' @exportMethod make_cocktail
#' 
setGeneric("make_cocktail",
        function(shaker, vegtable, ...)
            standardGeneric("make_cocktail")
)

#' @rdname make_cocktail
#' 
#' @aliases make_cocktail,shaker,vegtable-method
#' 
setMethod("make_cocktail", signature(shaker="shaker", vegtable="vegtable"),
        function(shaker, vegtable, which, cover, syntax="Syntax", FUN=sum,
				...) {
            # Build pseudo-species
            if(length(shaker@pseudos) > 0)
				for(i in 1:length(shaker@pseudos))
					vegtable <- merge_taxa(vegtable, shaker@pseudos[[i]])
			# Insert concept IDs in samples
            vegtable@samples$TaxonConceptID <- vegtable@species@taxonNames[
                    match(vegtable@samples$TaxonUsageID,
                            vegtable@species@taxonNames$TaxonUsageID),
                    "TaxonConceptID"]
            # Check presence of groups
            OUT <- list()
            if(length(shaker@groups) > 0) {
                OUT$groups <- list()
                PA <- aggregate(as.formula(paste(cover,
                                        "~ ReleveID + TaxonConceptID")),
                        vegtable@samples, function(x) {
                            x <- sum(x)
                            if(x > 0) x <- 1
                            return(x)
                        }
                )
                for(i in 1:length(shaker@groups)) {
                    PA_aux <- PA[PA$TaxonConceptID %in% shaker@groups[[i]],]
                    PA_aux <- aggregate(as.formula(paste(cover, "~ ReleveID")),
                            PA_aux, sum)
                    PA_aux <- PA_aux[PA_aux[,cover] >=
                                    length(shaker@groups[[i]])/2,]
                    OUT$groups[[i]] <- vegtable@header$ReleveID %in%
                            PA_aux$ReleveID
                }
            }
            if(!is.null(names(shaker@groups))) names(OUT$groups) <-
						names(shaker@groups)
            # Check for dominants
            if(nrow(shaker@dominants) > 0) {
                OUT$dominants <- list()
                DOM <- aggregate(as.formula(paste(cover,
                                        "~ ReleveID + TaxonConceptID")),
                        vegtable@samples, FUN)
                for(i in 1:nrow(shaker@dominants)) {
                    DOM_aux <- DOM[DOM$TaxonConceptID == shaker@dominants[i,
                                    "TaxonConceptID"],]
                    DOM_aux <- DOM_aux[
                            eval(parse(text=paste(c(cover, shaker@dominants[i,
															c("operator",
																	"value")]),
                                                    collapse=" ")), DOM_aux),]
                    OUT$dominants[[i]] <- vegtable@header$ReleveID %in%
                            DOM_aux$ReleveID
                }
            }
            # Excecute formulas
            OUT$units <- list()
			if(!missing(which))
				which <- which[which %in% names(shaker@formulas)] else
				which <- names(shaker@formulas)
			for(i in which)
				OUT$units[[i]] <-
						as.numeric(eval(parse(text=shaker@formulas[[i]]), OUT))
			OUT <- as.data.frame(OUT$units, stringsAsFactors=FALSE)
            SYNTAX <- rep(NA, nrow(OUT))
            for(i in colnames(OUT)) {
                vegtable@header[,i] <- OUT[,i]
                SYNTAX[OUT[,i] == 1] <- i
            }
            SYNTAX[rowSums(OUT) > 1] <- "+"
            vegtable@header[,syntax] <- SYNTAX
            return(vegtable@header)
        }
)
