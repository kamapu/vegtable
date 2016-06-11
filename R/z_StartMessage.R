# TODO:   Message displayed on start (modified from vegdata)
# 
# Author: Miguel Alvarez
################################################################################

.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is vegtables ",
            utils::packageDescription("vegtables", field="Version"),
            appendLF=TRUE)
}
