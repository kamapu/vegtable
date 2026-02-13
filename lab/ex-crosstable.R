# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)

veg <- cover_trans(subset(Kenya_veg, REFERENCE == 2331, slot = "header"),
    to = "cover", rule = "middle"
)

ct <- crosstable(cover ~ ReleveID + AcceptedName, FUN = sum,
    data = veg[c(1:10),],
    na_to_zero = TRUE)
ct

