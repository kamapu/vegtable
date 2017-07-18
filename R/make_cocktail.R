# TODO:   Carry out a Cocktail classification
# 
# Author: Miguel Alvarez
################################################################################

setGeneric("make_cocktail",
        function(shaker, vegtable, ...)
            standardGeneric("make_cocktail")
)

# Method for 'taxlist' objects
setMethod("make_cocktail", signature(shaker="shaker", vegtable="vegtable"),
        function(shaker, vegtable, which, cover, syntax="Syntax", FUN=sum, ...) {
            # Build pseudo-species
            if(length(shaker@pseudos) > 0)
                for(i in 1:length(shaker@pseudos)) {
                    vegtable@species@taxonNames[
                            vegtable@species@taxonNames$TaxonConceptID %in%
                                    shaker@pseudos[[i]],
                            "TaxonConceptID"] <- shaker@pseudos[[i]][1]
                    vegtable@species@taxonRelations <-
                            vegtable@species@taxonRelations[
                                    !vegtable@species@taxonRelations$TaxonConceptID %in%
                                            shaker@pseudos[[i]][-1],]
                    vegtable@species <- clean(vegtable@species)
                }
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
            if(!is.null(names(shaker@groups))) names(OUT$groups) <- names(shaker@groups)
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
                            eval(parse(text=paste(c(cover, shaker@dominants[i, c("operator", "value")]),
                                                    collapse=" ")), DOM_aux),]
                    OUT$dominants[[i]] <- vegtable@header$ReleveID %in%
                            DOM_aux$ReleveID
                }
            }
            # Excecute formulas
            OUT$units <- list()
            for(i in 1:length(shaker@formulas))
                OUT$units[[i]] <- as.numeric(eval(parse(text=shaker@formulas[[i]]), OUT))
            # Output (head of vegtable)
            if(is.null(names(shaker@formulas))) {
                names(OUT$units) <- paste0("unit_", 1:length(shaker@formulas))
            } else names(OUT$units) <- names(shaker@formulas)
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
