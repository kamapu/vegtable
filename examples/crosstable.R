# Produce a subset
veg <- subset(Kenya_veg, REFERENCE == 2331, slot = "header")

## transform cover to percentage
veg <- cover_trans(veg, to = "cover_perc", rule = "middle")

## cross table of the first 5 plots
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
  data = veg[1:5, ], FUN = mean, na_to_zero = TRUE)
head(Cross)

## cross table of recorded genera
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
    data = veg, FUN = mean, level = "genus")
head(Cross[ , 1:7])

## cross table of data merged to genus
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
    data = veg, FUN = sum, level = "genus", include_lower = TRUE)
head(Cross[ , 1:7])

## the same for families
Cross <- crosstable(cover_perc ~ ReleveID + AcceptedName + AuthorName,
    data = veg, FUN = sum, level = "family", include_lower = TRUE)
head(Cross[ , 1:7])
