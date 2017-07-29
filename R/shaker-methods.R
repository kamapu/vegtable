# TODO:   Methods applied to modify the shaker
# 
# Author: Miguel Alvarez
################################################################################

# Function setting species groups ----------------------------------------------
setGeneric("set_group",
        function(shaker, content, group, ...)
            standardGeneric("set_group")
)

# Method for 'taxlist' objects
setMethod("set_group", signature(shaker="shaker", content="taxlist",
                group="character"),
        function(shaker, content, group, group_id, authority=FALSE,
				enc_cont="latin1", enc_gr="utf8", ...) {
            content <- accepted_name(content)
			if(authority) {
				content$AuthorName[is.na(content$AuthorName)] <- ""
				content$TaxonName <- with(content, paste(TaxonName, AuthorName))
			}
			content$TaxonName <- iconv(content$TaxonName, enc_cont,
					"ASCII//TRANSLIT")
			group <- iconv(group, enc_gr, "ASCII//TRANSLIT")
			if(any(!group %in% content$TaxonName))
                warning("Some names in 'group' are not in 'content'")
            if(any(duplicated(content$TaxonName)))
                warning("Some duplicated names in 'content', only one will be retrieved")
            if(missing(group_id))
                group_id <- length(shaker@groups) + 1
            shaker@groups[[group_id]] <- content[charmatch(group,
                            content$TaxonName),"TaxonConceptID"]
            return(shaker)
        }
)

# Method for 'vegtable' objects
setMethod("set_group", signature(shaker="shaker", content="vegtable",
                group="character"),
        function(shaker, content, group, ...) {
            set_group(shaker, content@species, group, ...)
        }
)

# Function setting pseudos -----------------------------------------------------
setGeneric("set_pseudo",
        function(shaker, content, pseudo, ...)
            standardGeneric("set_pseudo")
)

# Method for 'taxlist' objects
setMethod("set_pseudo", signature(shaker="shaker", content="taxlist",
                pseudo="character"),
        function(shaker, content, pseudo, pseudo_id, authority=FALSE,
				enc_cont="latin1", enc_gr="utf8", ...) {
            content <- accepted_name(content)
			if(authority) {
				content$AuthorName[is.na(content$AuthorName)] <- ""
				content$TaxonName <- with(content, paste(TaxonName, AuthorName))
			}
			content$TaxonName <- iconv(content$TaxonName, enc_cont,
					"ASCII//TRANSLIT")
			group <- iconv(group, enc_gr, "ASCII//TRANSLIT")
			if(any(!pseudo %in% content$TaxonName))
                warning("Some names in 'pseudo' are not in 'content'")
            if(any(duplicated(content$TaxonName)))
                warning("Some duplicated names in 'content', only one will be retrieved")
            if(missing(pseudo_id))
                pseudo_id <- length(shaker@pseudos) + 1
            shaker@pseudos[[pseudo_id]] <- content[charmatch(pseudo,
                            content$TaxonName),"TaxonConceptID"]
            return(shaker)
        }
)

# Method for 'vegtable' objects
setMethod("set_pseudo", signature(shaker="shaker", content="vegtable",
                pseudo="character"),
        function(shaker, content, pseudo, ...) {
            set_pseudo(shaker, content@species, pseudo, ...)
        }
)

# Function setting formulas ----------------------------------------------------
setGeneric("set_formula",
		function(shaker, content, formula, ...)
			standardGeneric("set_formula")
)

# Method for 'taxlist' objects
setMethod("set_formula", signature(shaker="shaker", content="taxlist",
				formula="character"),
		function(shaker, content, formula, formula_id, authority=FALSE,
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
				stop("Some groups mentioned in the formula are not included in 'shaker'")
			# Retrieve
			content <- accepted_name(content)
			if(authority) {
				content$AuthorName[is.na(content$AuthorName)] <- ""
				content$TaxonName <- with(content, paste(TaxonName, AuthorName))
			}
			dominants <- strsplit(Names[Slots == "species"], " ")
			# function to merge species names elements
			dominants <- as.data.frame(do.call(rbind, lapply(dominants,
									format_F1)), stringsAsFactors=FALSE)
			colnames(dominants) <- c("TaxonConceptID", "operator", "value")
			# In case of use of authority
			content$TaxonName <- iconv(content$TaxonName, enc_cont,
					"ASCII//TRANSLIT")
			dominants$TaxonConceptID <- iconv(dominants$TaxonConceptID, enc_gr,
					"ASCII//TRANSLIT")
			dominants$TaxonConceptID <- content[match(dominants$TaxonConceptID,
							content$TaxonName),"TaxonConceptID"]
			dominants$value <- as.numeric(dominants$value)
			# paste rows in Names before continuing
			Names[Slots == "species"] <- formula_new <- apply(dominants, 1,
					format_F2)
			# merge slot dominants and extract duplicated
			formula_old <- apply(shaker@dominants, 1,format_F2)
			dominants <- do.call(rbind, list(shaker@dominants,
							dominants[!formula_new %in% formula_old,]))
			rownames(dominants) <- NULL
			formula_new <- apply(dominants, 1, format_F2)
			# reformat formula
			Names[Slots == "species"] <- paste(match(Names[Slots == "species"],
							formula_new))
			Names[Slots == "groups"] <- paste0(SYM, Names[Slots == "groups"],
					SYM)
			# Assemble new formulas
			Names_old <- paste0(Slots, ":", SYM, Names_old, SYM)
			Names <- paste0(Slots, "[[", Names, "]]")
			for(i in 1:length(Names))
				formula <- sub(Names_old[i], Names[i], formula)
			# Insert results in output object
			shaker@dominants <- dominants
			if(missing(formula_id))
				formula_id <- length(shaker@formulas) + 1
			shaker@formulas[[formula_id]] <- formula
			return(shaker)
		}
)

# Method for 'vegtable' objects
setMethod("set_formula", signature(shaker="shaker", content="vegtable",
				formula="character"),
		function(shaker, content, formula, ...) {
			set_formula(shaker, content@species, formula, ...)
		}
)

# TODO: summary method

# TODO: print method
