### spatial methods

#	bbox method
#	warning: returns bbox only for SpatialPoints!
setMethod("bbox",
	signature(obj = "VegsoupData"),
    function (obj) bbox(obj@sp.points)
)

#	coordinates method
setMethod("coordinates",
	signature(obj = "VegsoupData"),
    function (obj) coordinates(obj@sp.points)
)

setReplaceMethod("coordinates",
	signature(object = "VegsoupData", value = "ANY"),
	function (object, value) 
		stop("setting coordinates cannot be done on Spatial objects, where they have already been set")
)
		
#	proj4string method
setMethod("proj4string",
	signature(obj = "VegsoupData"),
	function (obj) proj4string(obj@sp.points)
)

setReplaceMethod("proj4string",
	signature(obj = "VegsoupData", value = "character"),
	function (obj, value) {
		value <- CRS(value)
		proj4string(obj@sp.points) <- value
		obj
	}
)

setMethod("spTransform",
	signature(x = "VegsoupData", "CRS"),
	function (x, CRSobj, ...) {
		require(rgdal)
		x@sp.points <- spTransform(x@sp.points, CRSobj, ...)
		x@sp.polygons <- spTransform(x@sp.polygons, CRSobj, ...)
		x	
	}
	
)
#	get spatial points
setGeneric("SpatialPointsVegsoup",
	function (obj)
		standardGeneric("SpatialPointsVegsoup")
)
setMethod("SpatialPointsVegsoup",
    signature(obj = "VegsoupData"),
    function (obj) obj@sp.points
)
#if (!isGeneric("SpatialPoints<-")) {
#setGeneric("SpatialPointsVegsoup<-",
#	function (obj, value)
#		standardGeneric("SpatialPointsVegsoup<-")
#)
#}
#setReplaceMethod("SpatialPointsVegsoup",
#	signature(obj = "VegsoupData", value = "SpatialPointsDataFrame"),
#	function (obj, value) {
#		#	to do: needs checking of plot names
#		obj@sp.points <- value
#		return(obj)		
#	}
#)

#	get spatial polygons
#if (!isGeneric("SpatialPolygons")) {
setGeneric("SpatialPolygonsVegsoup",
	function (obj)
		standardGeneric("SpatialPolygonsVegsoup")
)
#}
setMethod("SpatialPolygonsVegsoup",
    signature(obj = "VegsoupData"),
    function (obj) obj@sp.polygons
)
#if (!isGeneric("SpatialPolygons"))
#setGeneric("SpatialPolygonsVegsoup<-",
#	function (obj, value)
#		standardGeneric("SpatialPolygonsVegsoup<-")
#)

#setReplaceMethod("SpatialPolygonsVegsoup",
#	signature(obj = "VegsoupData", value = "SpatialPolygonsDataFrame"),
#	function (obj, value) {
#		#	to do: needs checking of plot names
#		obj@sp.polygons <- value
#		return(obj)		
#	}
#)