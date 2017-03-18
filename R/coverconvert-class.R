# TODO:   Defining a new class for conversion tables
# 
# Author: Miguel Alvarez
################################################################################

# Definition of the class
setClass("coverconvert",
        slots=c(
                value="list",
                conversion="list"
                ),
        prototype=list(
                value=list(),
                conversion=list()
                ),
        validity=function(object) {
            if(length(object@value) > 0)
                # For whole object
                if(!all(names(object@value) == names(object@conversion)))
                    return("Names in slots 'value' and 'conversion' do not match")
                # For single scales
                for(i in names(object@value)) {
                    if(length(object@value[[i]]) !=
                            length(object@conversion[[i]]) - 1)
                        return(paste0("Invalid length of vectors in scale '", i, "'"))
                    if(!is.numeric(object@conversion[[i]]))
                        return(paste0("Values of 'conversion' in scale '", i, "' have to be numeric"))
                    if(!is.character(object@value[[i]]))
                        return(paste0("Values of 'value' in scale '", i, "' have to be numeric"))
                }
        }
)
