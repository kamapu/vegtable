# TODO:   Summary methods for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

setMethod(f="summary", signature(object="vegtable"),
        function(object, ...) {
            # Show original attributes (metadata)
            if(length(object@description) > 0) for(i in names(object@description)) {
                    cat(i, ": ", object@description[i], sep="", "\n")
                }
            cat("\n")
            # Show dimensions of database
            cat(dim(object@head)[1], " observations (plots).", sep="", "\n")
            cat(dim(object@head)[2], " variables with records.", sep="", "\n")
            cat("\n")
            # Show summary of species list
            cat("Summary of species list:", sep="", "\n")
            if(class(object@species) == "taxlist") summary(object@species)
            # Show logged actions
            if(length(object@log$import) > 0) cat("[", object@log$import["time"],
                        "]: data imported from '", object@log$import["database"],
                        "'", sep="", "\n")
            cat("\n")
            if(length(object@log$deleted.plots) > 0) {
                cat("Plots deleted from original", sep="", "\n")
                for(i in 1:length(object@log$deleted.plots)) {
                    if(length(object@log$deleted.plots[[i]]) > 50) {
                        object@log$deleted.plots[[i]] <-
                                c(object@log$deleted.plots[[i]][1:50],
                                        "... [TRUNCATED]")
                    }
                    cat(paste0("[", attr(object@log$deleted.plots, "time")[i], "]:",
                                    collapse=""), object@log$deleted.plots[[i]],
                            sep=" ", fill=TRUE)
                }
            }
            cat("\n")
            if(length(object@log$deleted.species) > 0) {
                cat("Species deleted from original", sep="", "\n")
                for(i in 1:length(object@log$deleted.species)) {
                    if(length(object@log$deleted.species[[i]]) > 50) {
                        object@log$deleted.species[[i]] <-
                                c(object@log$deleted.species[[i]][1:50],
                                        "... [TRUNCATED]")
                    }
                    cat(paste0("[", attr(object@log$deleted.species,
                                            "time")[i], "]:", collapse=""),
                            object@log$deleted.species[[i]], sep=" ", fill=TRUE)
                }
            }
            cat("\n")
            if(length(object@log$replaced.species) > 0) {
                cat("Species replaced by 'poolssp'", sep="", "\n")
                for(i in 1:length(object@log$replaced.species)) {
                    if(length(object@log$replaced.species[[i]]) > 50) {
                        object@log$replaced.species[[i]] <-
                                c(object@log$replaced.species[[i]][1:50],
                                        "... [TRUNCATED]")
                    }
                    cat(paste0("[", attr(object@log$replaced.species,
                                            "time")[i], "]:", collapse=""),
                            object@log$replaced.species[[i]], sep=" / ", fill=TRUE)
                }
            }
            cat("\n")
        })
