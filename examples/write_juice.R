## Subset and transform cover values to percentage
vegetation <- Kenya_veg[1:20, ]
vegetation <- cover_trans(x = vegetation, to = "cover_percent", rule = "middle")

## Write in tempdir
write_juice(data = vegetation, file = file.path(tempdir(), "SWEA"),
    formula = cover_percent ~ ReleveID + AcceptedName, FUN = mean,
    header = c("ReleveID", "COMM_TYPE"))
