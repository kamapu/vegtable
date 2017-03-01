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

sweadataveg <- import_vegtable("Sweadataveg")
sweadataveg <- subset_by_popup(sweadataveg, "tvrefenc", paste(REFERENCE) %in%
                paste(c(5,6,7,9,12)))

validObject(sweadataveg)
save(sweadataveg, file="M:/WorkspaceEclipse/vegtable/data/sweadataveg.rda")
