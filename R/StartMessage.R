# Start message
.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is vegtable ",
            utils::packageDescription("vegtable", field="Version"),
            appendLF=TRUE)
}
