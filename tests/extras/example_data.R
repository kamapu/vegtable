# TODO:   Importing data from Turboveg
# 
# Author: Miguel Alvarez
################################################################################

# SELECTED REFERENCES:
# 5: Bussmann (2002)
# 6: Schmitt (1991)
# 7: Bussmann (1994)
# 9: Bronner (1990)
# 12: Fujiwara et al. (2014)

Kenya_veg <- import_vegtable("Kenya_veg")
Kenya_veg <- subset_by_popup(Kenya_veg, "tvrefenc", paste(REFERENCE) %in%
                paste(c(5,6,7,9,12)))

validObject(Kenya_veg)
save(Kenya_veg, file="M:/WorkspaceEclipse/vegtable/data/Kenya_veg.rda")
