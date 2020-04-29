#' @name vegtable2kml
#' 
#' @title Mapping of plot observations
#' 
#' @description 
#' This function is a wrapper of [plotKML::kml()] producing and displaying KML
#' files.
#' 
#' Georeferenced plots can be quickly displayed in
#' [Google Earth](https://www.google.com/intl/en_us/earth/) using this function.
#' 
#' @param obj Input object containing coordinate values.
#' @param file Character value with the name of output file (including file
#'     extension).
#' @param coords Either a character vector or a formula indicating the names of
#'     coordinate values.
#' @param srs Spatial reference system as `proj4string`.
#' @param ... Further arguments passed among methods.
#' 
#' @return
#' A KML file, which will be automatically opened in **Google Earth**.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## Plots containing Podocarpus observations
#' Kenya_veg@species <- subset(Kenya_veg@species, grepl("Podocarpus", TaxonName),
#' 	   slot="names")
#' 	
#' Kenya_veg <- subset(Kenya_veg, TaxonUsageID %in%
#' 	   Kenya_veg@species@taxonNames$TaxonUsageID, slot="samples")
#' 
#' \dontrun{vegtable2kml(Kenya_veg, "Podocarpus.kml")}
#' 
#' @rdname vegtable2kml
#' 
#' @exportMethod vegtable2kml
#' 
setGeneric("vegtable2kml", function(obj, ...)
            standardGeneric("vegtable2kml"))

#' @rdname vegtable2kml
#' 
#' @aliases vegtable2kml,data.frame-method
#' 
setMethod("vegtable2kml", signature(obj="data.frame"),
        function(obj, file, coords=~ Longitude + Latitude,
                srs=CRS("+proj=longlat +datum=WGS84")) {
            # to SpatialPointsDataFrame
            coordinates(obj) <- coords
            proj4string(obj) <- srs
            obj <- reproject(obj)
            # write and show
            kml(obj, shape=paste0("http://maps.google.com/mapfiles/kml/",
							"shapes/placemark_circle.png"),
                    colour="yellow", size=1, file=file)
            kml_View(file)
        }
)

#' @rdname vegtable2kml
#' 
#' @aliases vegtable2kml,vegtable-method
#' 
setMethod("vegtable2kml", signature(obj="vegtable"),
        function(obj, file, coords=~ LONGITUDE + LATITUDE,
                srs=CRS("+proj=longlat +datum=WGS84")) {
            obj <- header(obj)
			rownames(obj) <- paste(obj$ReleveID)
            vegtable2kml(obj, file, coords, srs)
        }
)
