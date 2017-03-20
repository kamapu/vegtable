# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)
library(vegan)

## Load data from vegan
data(dune)
data(dune.env)
data(braun_blanquet)

## Conversion to vegtable
dune_veg <- data.frame(species=colnames(dune), t(dune), stringsAsFactors=FALSE,
        check.names=FALSE)
dune_veg <- df2vegtable(dune_veg, species=1)

colnames(dune_veg@samples)[colnames(dune_veg@samples) == "Cover"] <- "ordin."
dune_veg@samples$ordin. <- factor(dune_veg@samples$ordin.,
        levels=levels(braun_blanquet@value$ordin.))
dune_veg@coverconvert <- braun_blanquet

# Adding environmental variables
dune.env$ReleveID <- as.integer(rownames(dune.env))
header(dune_veg) <- dune.env

# Cleaning object and getting percentage cover
dune_veg@samples <- dune_veg@samples[!is.na(dune_veg@samples$ordin.),]
dune_veg <- clean(dune_veg)
summary(dune_veg)

# Save data set
save(dune_veg, file="M:/WorkspaceEclipse/vegtable/data/dune_veg.rda")
