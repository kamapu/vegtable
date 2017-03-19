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

validObject(Kenya_veg)

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

save(Kenya_veg, file="M:/WorkspaceEclipse/vegtable/data/Kenya_veg.rda")
