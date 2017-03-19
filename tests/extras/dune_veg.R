# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)
library(vegan)

## Load data from vegan
data(dune)
data(dune.env)

## Conversion to vegtable
dune_veg <- data.frame(species=colnames(dune), t(dune), stringsAsFactors=FALSE,
        check.names=FALSE)
dune_veg <- df2vegtable(dune_veg, species=1)

# Adding environmental variables
dune.env$ReleveID <- as.integer(rownames(dune.env))
header(dune_veg) <- dune.env

summary(dune_veg)

# Save data set
save(dune_veg, file="M:/WorkspaceEclipse/vegtable/data/dune_veg.rda")
