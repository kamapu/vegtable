# Produce a subset
veg <- subset(Kenya_veg, REFERENCE == 2331, slot = "header")

## transform cover to percentage
veg <- cover_trans(veg, to = "cover_perc", rule = "middle")

## cross table of the first 5 plots
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
  veg[1:5, ], mean,
  na_to_zero = TRUE
)
head(Cross)
