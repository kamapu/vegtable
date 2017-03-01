# TODO:   Methods to retrieve or replace relations
# 
# Author: Miguel Alvarez
################################################################################

# Generic function and vegtable method -----------------------------------------
setGeneric("veg_relation",
        function(vegtable, relation, ...)
            standardGeneric("veg_relation")
)

# Set method for vegtable
setMethod("veg_relation", signature(vegtable="vegtable", relation="character"),
        function(vegtable, relation, ...) return(vegtable@relations[[relation]])
)

# Replacement method -----------------------------------------------------------
setGeneric("veg_relation<-", function(vegtable, relation, value)
            standardGeneric("veg_relation<-"))

# Definition of method
setReplaceMethod("veg_relation", signature(vegtable="vegtable",
                relation="character", value="data.frame"),
        function(vegtable, relation, value) {
            # First the pre-tests
            VAR <- colnames(value)[1]
            if(!VAR %in% colnames(vegtable@head))
                stop("The first column in 'value' is not present in slot 'head'")
            if(sum(VAR %in% colnames(vegtable@head)) > 1)
                stop("The target variable has duplicates in 'head'")
            if(any(duplicated(paste(value[,VAR]))))
                stop("The new popup contains duplicated levels", call.=FALSE)
            Var1 <- unique(paste(vegtable@head[,VAR]))
            Var1 <- Var1[!is.na(Var1)]
            Var2 <- paste(value[,VAR])
            all(Var1 %in% Var2)
            # Factorize
            vegtable@head[,VAR] <- factor(paste(vegtable@head[,VAR]),
                    levels=paste(value[,VAR]))
            value[,VAR] <- factor(paste(value[,VAR]), levels=paste(value[,VAR]))
            vegtable@relations[[relation]] <- value
            return(vegtable)
        }
)
