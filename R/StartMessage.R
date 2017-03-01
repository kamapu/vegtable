# TODO:   Message displayed on start (modified from vegdata)
# 
# Author: Miguel Alvarez
################################################################################

.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is vegtable ",
            utils::packageDescription("vegtable", field="Version"),
            appendLF=TRUE)
}
