# TODO:   Methods to retrieve or replace popup lists
# 
# Author: Miguel Alvarez
################################################################################

# Generic function and vegtable method -----------------------------------------
setGeneric("popup",
        function(vegtable, popup, ...)
            standardGeneric("popup")
)

# Set method for vegtable
setMethod("popup", signature(vegtable="vegtable", popup="character"),
        function(vegtable, popup, ...) return(vegtable@popups[[popup]])
)

# Replacement method -----------------------------------------------------------
setGeneric("popup<-", function(vegtable, popup, value)
            standardGeneric("popup<-"))

# Definition of method
setReplaceMethod("popup", signature(vegtable="vegtable", popup="character",
                value="data.frame"),
        function(vegtable, popup, value) {
            # First the pre-tests
            VAR <- colnames(value)[1]
            if(!VAR %in% colnames(vegtable@head))
                stop("The first column in 'value' is not present in slot 'head'",
                        call.=FALSE)
            if(sum(VAR %in% colnames(vegtable@head)) > 1)
                stop("The target variable has duplicates in 'head'",
                        call.=FALSE)
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
            vegtable@popups[[popup]] <- value
            return(vegtable)
        })
