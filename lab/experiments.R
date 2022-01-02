# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtable")

library(vegtable)

## Test object without samples
KE1 <- KE2 <- Kenya_veg
summary(KE2)

# first 500 plots without records
KE1@samples <- KE1@samples[!KE1@samples$ReleveID %in%
				KE1@header$ReleveID[1:500], ]
summary(KE1)

# No records at all
KE2@samples <- KE2@samples[0, ]
summary(KE2)

## Subset by taxon name --------------------------------------------------------
Kenya_sub <- subset(x=Kenya_veg, subset=TaxonName == "Tagetes",
		slot="taxonNames", keep_children=TRUE, keep_parents=TRUE)
summary(Kenya_sub)
summary(Kenya_sub@species)

## Subset by taxon relations
Kenya_sub <- subset(x=Kenya_veg, subset=Level == "species",
		slot="taxonRelations")
summary(Kenya_sub)
summary(Kenya_sub@species)

## Subset by taxon traits
Kenya_sub <- subset(x=Kenya_veg, subset=lf_behn_2018 == "obligate_annual",
		slot="taxonTraits")
summary(Kenya_sub)
summary(Kenya_sub@species)

## Subset by header
Kenya_sub <- subset(x=Kenya_veg, subset=ALTITUDE <= 1000, slot="header")
summary(Kenya_sub)

## Subset by samples (after converting coverage)
Kenya_veg <- transform(x=Kenya_veg, to="cover_percentage", rule="middle")
Kenya_sub <- subset(x=Kenya_veg, subset=cover_percentage >= 50, slot="samples")
summary(Kenya_sub)

## Subset by relations
Kenya_sub <- subset(x=Kenya_veg, subset=as.integer(YEAR) >= 2000,
		slot="relations", relation="REFERENCE")
summary(Kenya_sub)








# by taxon 



summary(Test)



x <- Kenya_veg
subset <- substitute(REFERENCE == 262)
slot="relations"
relation="REFERENCE"

p_slots <- c("taxonNames", "taxonRelations", "taxonTraits",
		"header", "samples", "relations")

object <- x

Test <- clean(object)

summary(Test)





Test <- clean_once(object)

Test <- clean_once(Test)

Test <- clean(object)

object@header <- object@header[ ,(colnames(object@header) == "ReleveID") |
				(!apply(object@header, 2, function(x) all(is.na(x)))),
		drop=FALSE]

summary(object)



