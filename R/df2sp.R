# TODO:   Mapping Objects
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("df2sp", function(x, ...)
            standardGeneric("df2sp"))

# Method for data frames
setMethod("df2sp", signature(x="data.frame"), function(x, lon="Longitude",
        lat="Latitude", file, ...) {
            coordinates(x) <- c(lon,lat)
            proj4string(x) <- CRS("+proj=longlat +datum=WGS84")
            if(missing(file)) return(x) else {
                writeOGR(Map, file, layer="plots", driver="KML", ...)
                shell.exec(file)
            }
        }
)

# Method for vegtables
setMethod("df2sp", signature(x="vegtable"), function(x, lon="LONGITUDE",
        lat="LATITUDE", ...) {
    df2sp(x@head, lon, lat, ...)
})
