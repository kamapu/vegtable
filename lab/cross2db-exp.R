# TODO:   Test function cross2db
# 
# Author: Miguel Alvarez
################################################################################

## devtools::install()
library(vegtable)

load("lab/lalashan.RData")

# Creates a data frame
obs_df <- as.data.frame(t(as.matrix(lalashan$spe)))
obs_df <- cbind(data.frame(species_code = rownames(obs_df)), obs_df)

dbl_1 <- cross2db(obs_df, na_strings = 0)
head(dbl_1)

# for matrix method
obs_m <- as.matrix(lalashan$spe)

dbl_2 <- cross2db(obs_m, na_strings = 0)
head(dbl_2)

# Failing example --------------------------------------------------------------
library(vegtable)

# New table
samples <- as.data.frame(readRDS("lab/samples-veg.rds"))
head(samples)

# Generate database (data.frame method)
samples_l <- cross2db(samples)
head(samples_l)

# Split cover
samples_l <- cross2db(samples, split_cover = ".")
head(samples_l)

# Using formula method
samples_l <- cross2db(~species, data = samples, split_cover = ".")
head(samples_l)
