# TODO:   Mapping Objects
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("vegtable2kml", function(obj, ...)
            standardGeneric("vegtable2kml"))

# Method for data frames
setMethod("vegtable2kml", signature(obj="data.frame"),
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
setMethod("vegtable2kml", signature(obj="vegtable"),
        function(obj, file, coords=~ LONGITUDE + LATITUDE,
                srs=CRS("+proj=longlat +datum=WGS84")) {
            obj <- header(obj)
            vegtable2kml(obj, file, coords, srs)
        }
)
