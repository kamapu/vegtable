# TODO:   Inserting relation variables to header slot
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("relation2header",
		function(vegtable, relation, ...)
			standardGeneric("relation2header")
)

# Set method for vegtable and data.frame
setMethod("relation2header", signature(vegtable="vegtable", relation="data.frame"),
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

# Set method for vegtable and character
setMethod("relation2header", signature(vegtable="vegtable", relation="character"),
		function(vegtable, relation, ...) {
			return(relation2header(vegtable, vegtable@relations[[relation]],
							by=relation, ...))
		}
)
