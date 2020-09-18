# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)

summary(Kenya_veg)

Test <- subset(Kenya_veg, REFERENCE == 262, slot="relations",
		relation="REFERENCE")



summary(Test)



x <- Kenya_veg
subset <- substitute(REFERENCE == 262)
slot="relations"
relation="REFERENCE"

p_slots <- c("taxonNames", "taxonRelations", "taxonTraits",
		"header", "samples", "relations")

object <- x


object@header <- object@header[ ,(colnames(object@header) == "ReleveID") |
				(!apply(object@header, 2, function(x) all(is.na(x)))),
		drop=FALSE]

summary(object)



