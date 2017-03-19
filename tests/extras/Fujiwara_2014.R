# TODO:   Fujiwara et al. (2014) as example data
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable2)
library(xlsx)
## setwd("M:/WorkspaceEclipse/vegtable_00/Fujiwara")

# Loading and preparing the data
data(Kenya_veg)
Kenya_veg <- subset(Kenya_veg, REFERENCE == 12, slot="header")

# Get species
Fujiwara_sp <- subset(Kenya_veg@species, TaxonUsageID %in%
                Kenya_veg@samples$TaxonUsageID, slot="names")
validObject(Fujiwara_sp)
Fujiwara_sp <- taxlist2tvlist(Fujiwara_sp)

for(i in names(Fujiwara_sp)) write.xlsx(Fujiwara_sp[[i]],
            paste0("M:/Programme/Turboveg_Miguel/excel2tvsplist/Fujiwara_", i,
                    ".xlsx"), row.names=FALSE)

# Get cross table and header
Fujiwara_sp$header <- Kenya_veg@header
Fujiwara_sp$samples <- crosstable(COVER_CODE ~ ReleveID + TaxonUsageID +
                LAYER, Kenya_veg@samples, paste0)
Fujiwara_sp$samples$TaxonUsageID <- Kenya_veg@species@taxonNames[
        match(Fujiwara_sp$samples$TaxonUsageID,
                Kenya_veg@species@taxonNames$TaxonUsageID),"TaxonName"]
colnames(Fujiwara_sp$samples)[1] <- "TaxonName"
SORT <- colnames(Fujiwara_sp$samples)[-c(1:2)]
SORT <- SORT[order(SORT)]
Fujiwara_sp$samples <- Fujiwara_sp$samples[,c("TaxonName","LAYER",SORT)]
Fujiwara_sp$header$ReleveID <- 1:nrow(Fujiwara_sp$header)
# Some further changes in header
Fujiwara_sp$header <- Fujiwara_sp$header[,
        apply(Fujiwara_sp$header, 2, function(x) !all(is.na(x)))]
Fujiwara_sp$header$DATE <- gsub("-", "", paste(Fujiwara_sp$header$DATE),
        fixed=TRUE)
Fujiwara_sp$header <- Fujiwara_sp$header[,colnames(Fujiwara_sp$header) !=
                "COVERSCALE"]

# Write data set
setwd("M:/WorkspaceEclipse/vegtable/inst/Fujiwara_2014")
for(i in names(Fujiwara_sp))
    write.xlsx(Fujiwara_sp[[i]], "Data.xlsx", sheetName=i, row.names=FALSE,
            showNA=FALSE, append=TRUE)
