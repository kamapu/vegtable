#' @name veg_relation
#' 
#' @title Retrieve or replace relations in vegtable objects
#' 
#' @description 
#' Tables providing information about levels of categorical variables in the
#' header of a **Turboveg** database are called `popups` in **Turboveg**,
#' but `relations` in [vegtable::vegtable].
#' Such variables will be converted into factors in the slot `header` according
#' to the levels and their sorting in the respective relation.
#' 
#' @param vegtable An object of class [vegtable-class].
#' @param relation A character value indicating the relation table to be
#'     retrieved or replaced.
#' @param by Character value indicating the name of the common column used as
#'     index for inserting values in slot `header`.
#' @param vars A character vector with the names of variables to be inserted in
#'     slot `header`.
#' @param match_header A logical vector, whether only levels occurring in slot
#'     `header` should be considered or all.
#' @param value A data frame containing the new veg_relation.
#' @param ... Further arguments to be passed among methods.
#' 
#' @return
#' This function retrieves and object of class `data.frame`.
#' In the replacement method, an object of class [vegtable-class], including
#' `value` in the slot `relations`.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## overview of references
#' veg_relation(Kenya_veg, "REFERENCE")
#' 
#' @rdname veg_relation
#' 
#' @exportMethod veg_relation
#' 
setGeneric("veg_relation",
        function(vegtable, relation, ...)
            standardGeneric("veg_relation")
)

#' @rdname veg_relation
#' 
#' @aliases veg_relation,vegtable,character-method
#' 
setMethod("veg_relation", signature(vegtable="vegtable", relation="character"),
        function(vegtable, relation, match_header=FALSE, ...)
			if(match_header)
				return(vegtable@relations[[relation]][
								vegtable@relations[[relation]][,relation] %in%
										vegtable@header[,relation],]) else 
				return(vegtable@relations[[relation]])
)

#' @rdname veg_relation
#' 
#' @aliases veg_relation<-
#' 
#' @exportMethod veg_relation<-
#' 
setGeneric("veg_relation<-", function(vegtable, relation, value)
            standardGeneric("veg_relation<-"))

#' @rdname veg_relation
#' 
#' @aliases veg_relation<-,vegtable,character,data.frame-method
#' 
setReplaceMethod("veg_relation", signature(vegtable="vegtable",
                relation="character", value="data.frame"),
        function(vegtable, relation, value) {
            # First the pre-tests
            VAR <- colnames(value)[1]
            if(!VAR %in% colnames(vegtable@header))
                stop("The first column in 'value' is not present in slot 'header'")
            if(sum(VAR %in% colnames(vegtable@header)) > 1)
                stop("The target variable has duplicates in 'header'")
            if(any(duplicated(paste(value[,VAR]))))
                stop("The new popup contains duplicated levels", call.=FALSE)
            Var1 <- unique(paste(vegtable@header[,VAR]))
            Var1 <- Var1[!is.na(Var1)]
            Var2 <- paste(value[,VAR])
            all(Var1 %in% Var2)
            # Factorize
            vegtable@header[,VAR] <- factor(paste(vegtable@header[,VAR]),
                    levels=paste(value[,VAR]))
            value[,VAR] <- factor(paste(value[,VAR]), levels=paste(value[,VAR]))
            vegtable@relations[[relation]] <- value
            return(vegtable)
        }
)

#' @rdname veg_relation
#' 
#' @aliases relation2header
#' 
#' @exportMethod relation2header
#' 
setGeneric("relation2header",
		function(vegtable, relation, ...)
			standardGeneric("relation2header")
)

#' @rdname veg_relation
#' 
#' @aliases relation2header,vegtable,data.frame-method
#' 
setMethod("relation2header", signature(vegtable="vegtable",
				relation="data.frame"),
		function(vegtable, relation, by, vars, ...) {
			if(!by %in% colnames(vegtable@header))
				stop("Argument 'by' is not a column in slot 'header'.")
			if(!by %in% colnames(relation))
				stop("Argument 'by' is not a column in table 'relation'.")
			if(missing(vars))
				vars <- colnames(relation)[colnames(relation) != by]
			for(i in vars)
				vegtable@header[,i] <- relation[match(vegtable@header[,by],
								relation[,by]),i]
			return(vegtable)
		}
)

#' @rdname veg_relation
#' 
#' @aliases relation2header,vegtable,character-method
#' 
setMethod("relation2header", signature(vegtable="vegtable", relation="character"),
		function(vegtable, relation, ...) {
			return(relation2header(vegtable, vegtable@relations[[relation]],
							by=relation, ...))
		}
)
