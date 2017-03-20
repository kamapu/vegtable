# TODO:   Generating example data from Turboveg
# 
# Author: Miguel Alvarez
################################################################################

# SELECTED REFERENCES:
# 2974: Bussmann (2002)
# 3011: Schmitt (1991)
# 3012: Bussmann (1994)
# 2331: Bronner (1990)
# 3506: Fujiwara et al. (2014)

library(vegtable)
## library(stringi)

Kenya_veg <- tv2vegtable("Sweadataveg", clean=FALSE)
Kenya_veg <- subset(Kenya_veg, paste(REFERENCE) %in% c("2974","3011","3012",
                "2331","3506"), slot="head")
data(braun_blanquet)
Kenya_veg@samples$br_bl <- factor(paste(Kenya_veg@samples$br_bl),
        levels=levels(braun_blanquet@value$br_bl))
Kenya_veg <- clean(Kenya_veg)

## Some issues regarding duplicated combinations
# 54195 to 50523 and 54248 to 54244
Kenya_veg@samples$TaxonUsageID[Kenya_veg@samples$TaxonUsageID == 54195] <- 50523
Kenya_veg@samples$TaxonUsageID[Kenya_veg@samples$TaxonUsageID == 54248] <- 54244
# Delete the usages
Kenya_veg@species@taxonNames <- Kenya_veg@species@taxonNames[
        !Kenya_veg@species@taxonNames$TaxonUsageID %in% c(54195,54248),]


# Eventual solution for non-ASCII strings in species names
for(i in c("TaxonName","AuthorName")) {
    Encoding(Kenya_veg@species@taxonNames[,i]) <- "latin1"
    Kenya_veg@species@taxonNames[,i] <- iconv(Kenya_veg@species@taxonNames[,i],
            "latin1", "UTF-8")
}
# Same problem in References
for(i in c("TITLE","PUBLISHED")) {
    Encoding(Kenya_veg@relations$REFERENCE[,i]) <- "latin1"
    Kenya_veg@relations$REFERENCE[,i] <- iconv(
            Kenya_veg@relations$REFERENCE[,i], "latin1", "UTF-8")
}

# Final object
Kenya_veg <- clean(Kenya_veg)
validObject(Kenya_veg)
save(Kenya_veg, file="M:/WorkspaceEclipse/vegtable/data/Kenya_veg.rda")
