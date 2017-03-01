# TODO:   Mapping Objects
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("obj2kml", function(obj, ...)
            standardGeneric("obj2kml"))

# Method for data frames
setMethod("obj2kml", signature(obj="data.frame"),
        function(obj, file, coords=~ Longitude + Latitude,
                srs=CRS("+proj=longlat +datum=WGS84")) {
            # to SpatialPointsDataFrame
            coordinates(obj) <- coords
            proj4string(obj) <- srs
            obj <- reproject(obj)
            # write and show
            kml(obj, shape="http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png",
                    colour="yellow", size=1, file=file)
            kml_View(file)
        }
)

# Method for vegtable objects
setMethod("obj2kml", signature(obj="vegtable"),
        function(obj, file, coords=~ LONGITUDE + LATITUDE,
                srs=CRS("+proj=longlat +datum=WGS84")) {
            obj <- header(obj)
            obj2kml(obj, file, coords, srs)
        }
)
