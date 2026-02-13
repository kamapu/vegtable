# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)

data(Kenya_veg)

data = Kenya_veg[1:10, ]
data$cluster <- rep_len(1:3, nrow(data@header))

trait_proportion(lf_behn_2018 ~ COUNTRY, object = data, in_header = FALSE)
trait_proportion(lf_behn_2018 ~ cluster, object = data, in_header = FALSE)

data <- trait_proportion(lf_behn_2018 ~ cluster, object = data)

data <- trait_proportion(lf_behn_2018 ~ ReleveID, object = data)




count_taxa(~ ReleveID, data = data, in_header = FALSE)
count_taxa(family ~ ReleveID, data = data, include_lower = TRUE,
    in_header = FALSE)

count_taxa(~ cluster, data = data, in_header = FALSE)
count_taxa(family ~ cluster, data = data, include_lower = TRUE,
    in_header = FALSE)

# Results in header
data <- count_taxa(~ ReleveID, data = data)
data <- count_taxa(family ~ ReleveID, data = data, include_lower = TRUE)

data <- count_taxa(~ cluster, data = data)
data <- count_taxa(family ~ cluster, data = data, include_lower = TRUE)






# Not working...
T1 <- trait_proportion(lf_behn_2018 ~ COUNTRY, object = Kenya_veg)
T1@relations$COUNTRY <- veg_relation(T1, "COUNTRY", match_header = TRUE)



