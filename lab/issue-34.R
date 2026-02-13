# TODO:   Solving issue with 'trait_stats()'
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)

releves <- readRDS("lab/sanmartin1998.rds")

wme <- function(x, w, na.rm = TRUE)
  mean(x*w, na.rm = na.rm)/
      sum(w, na.rm = na.rm)

releves <- trait_stats(
    object = releves,
    FUN = wme,
    trait = "ind_n",
    head_var = "ReleveID",
    weight = "cover_percentage",
    suffix = "_wme",
    in_header = TRUE)

summary(releves$ind_n_wme)

releves <- trait_stats(
    object = releves,
    FUN = wme,
    trait = "ind_n",
    head_var = "ReleveID",
    weight = "cover_percentage",
    suffix = "_wme2",
    in_header = TRUE,
    na.rm = FALSE)

summary(releves$ind_n_wme2)
