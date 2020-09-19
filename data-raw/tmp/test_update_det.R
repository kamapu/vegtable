# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)
library(vegtable2)

load_last("data-raw/tmp/sweadataveg_kai")
swea <- sweadataveg


Sel <- subset(swea, !is.na(kbehn_r), slot="samples")$ReleveID

Sel1 <- subset(swea, ReleveID %in% Sel)
Sel2 <- update_det(Sel1, "kbehn_r")

Sel <- subset(Sel1@samples, TaxonUsageID != Sel2@samples$TaxonUsageID)

crosstable(cover_percentage ~ ReleveID + AcceptedName, subset(Sel1,
				TaxonUsageID != Sel2@samples$TaxonUsageID, slot="samples"),
		mean)

crosstable(cover_percentage ~ ReleveID + AcceptedName, subset(Sel2,
				TaxonUsageID != Sel1@samples$TaxonUsageID, slot="samples"),
		mean)
