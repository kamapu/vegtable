# TODO:   Methods applied to modify the shaker
# 
# Author: Miguel Alvarez
################################################################################

# Function setting species groups ----------------------------------------------
setGeneric("set_group",
        function(shaker, companion, group, ...)
            standardGeneric("set_group")
)

# Method for 'taxlist' objects
setMethod("set_group", signature(shaker="shaker", companion="taxlist",
                group="character"),
        function(shaker, companion, group, group_id, authority=FALSE,
				enc_cont="latin1", enc_gr="utf8", ...) {
            companion <- accepted_name(companion)
			if(authority) {
				companion$AuthorName[is.na(companion$AuthorName)] <- ""
				companion$TaxonName <- with(companion, paste(TaxonName, AuthorName))
			}
			companion$TaxonName <- iconv(companion$TaxonName, enc_cont,
					"ASCII//TRANSLIT")
			group <- iconv(group, enc_gr, "ASCII//TRANSLIT")
			if(any(!group %in% companion$TaxonName))
                stop("Some names in 'group' are not in 'companion'")
			if(any(duplicated(group)))
				group <- unique(group)
            if(any(duplicated(companion$TaxonName)))
                warning("Some duplicated names in 'companion', only one will be retrieved")
            if(missing(group_id))
                group_id <- length(shaker@groups) + 1
            shaker@groups[[group_id]] <- companion[charmatch(group,
                            companion$TaxonName),"TaxonConceptID"]
            return(shaker)
        }
)

# Method for 'vegtable' objects
setMethod("set_group", signature(shaker="shaker", companion="vegtable",
                group="character"),
        function(shaker, companion, group, ...) {
            set_group(shaker, companion@species, group, ...)
        }
)

# Function setting pseudos -----------------------------------------------------
setGeneric("set_pseudo",
        function(shaker, companion, pseudo, ...)
            standardGeneric("set_pseudo")
)

# Method for 'taxlist' objects
setMethod("set_pseudo", signature(shaker="shaker", companion="taxlist",
                pseudo="character"),
        function(shaker, companion, pseudo, pseudo_id, authority=FALSE,
				enc_cont="latin1", enc_gr="utf8", ...) {
            companion <- accepted_name(companion)
			if(authority) {
				companion$AuthorName[is.na(companion$AuthorName)] <- ""
				companion$TaxonName <- with(companion, paste(TaxonName, AuthorName))
			}
			companion$TaxonName <- iconv(companion$TaxonName, enc_cont,
					"ASCII//TRANSLIT")
			pseudo <- iconv(pseudo, enc_gr, "ASCII//TRANSLIT")
			if(any(!pseudo %in% companion$TaxonName))
                stop("Some names in 'pseudo' are not in 'companion'")
            if(any(duplicated(companion$TaxonName)))
                warning("Some duplicated names in 'companion', only one will be retrieved")
            if(missing(pseudo_id))
                pseudo_id <- length(shaker@pseudos) + 1
            shaker@pseudos[[pseudo_id]] <- companion[charmatch(pseudo,
                            companion$TaxonName),"TaxonConceptID"]
            return(shaker)
        }
)

# Method for 'vegtable' objects
setMethod("set_pseudo", signature(shaker="shaker", companion="vegtable",
                pseudo="character"),
        function(shaker, companion, pseudo, ...) {
            set_pseudo(shaker, companion@species, pseudo, ...)
        }
)

# Function setting formulas ----------------------------------------------------
setGeneric("set_formula",
		function(shaker, companion, formula, ...)
			standardGeneric("set_formula")
)

# Method for 'taxlist' objects
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
				stop("Some groups mentioned in the 'formula' are not included in 'shaker'")
			# Retrieve
			companion <- accepted_name(companion)
			if(authority) {
				companion$AuthorName[is.na(companion$AuthorName)] <- ""
				companion$TaxonName <- with(companion, paste(TaxonName, AuthorName))
			}
			if(any(Slots == "species")) {
				dominants <- strsplit(Names[Slots == "species"], " ")
				# function to merge species names elements
				dominants <- as.data.frame(do.call(rbind, lapply(dominants,
										format_F1)), stringsAsFactors=FALSE)
				colnames(dominants) <- c("TaxonConceptID", "operator", "value")
				# In case of use of authority
				companion$TaxonName <- iconv(companion$TaxonName, enc_cont,
						"ASCII//TRANSLIT")
				dominants$TaxonConceptID <- iconv(dominants$TaxonConceptID,
						enc_gr, "ASCII//TRANSLIT")
				if(any(!dominants$TaxonConceptID %in% companion$TaxonName))
					stop("Some species in 'formula' are not included in 'companion'")
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

# Method for 'vegtable' objects
setMethod("set_formula", signature(shaker="shaker", companion="vegtable",
				formula="character"),
		function(shaker, companion, formula, ...) {
			set_formula(shaker, companion@species, formula, ...)
		}
)
