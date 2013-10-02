### spatial methods

#	bbox method
#	warning: returns bbox only for SpatialPoints!
setMethod("bbox",
	signature(obj = "Vegsoup"),
    function (obj) bbox(obj@sp.points)
)

#	coordinates method
setMethod("coordinates",
	signature(obj = "Vegsoup"),
    function (obj) coordinates(obj@sp.points)
)

setReplaceMethod("coordinates",
	signature(object = "Vegsoup", value = "ANY"),
	function (object, value) 
		stop("setting coordinates cannot be done on Spatial objects, where they have already been set")
)
		
#	proj4string method
setMethod("proj4string",
	signature(obj = "Vegsoup"),
	function (obj) proj4string(obj@sp.points)
)

setReplaceMethod("proj4string",
	signature(obj = "Vegsoup", value = "character"),
	function (obj, value) {
		value <- CRS(value)
		proj4string(obj@sp.points) <- value
		obj
	}
)

setMethod("spTransform",
	signature(x = "Vegsoup", "CRS"),
	function (x, CRSobj, ...) {
		require(rgdal)
		x@sp.points <- spTransform(x@sp.points, CRSobj, ...) # rgdal::
		x@sp.polygons <- spTransform(x@sp.polygons, CRSobj, ...) # rgdal::
		x	
	}
	
)
#	get spatial points
setGeneric("SpatialPointsVegsoup",
	function (obj)
		standardGeneric("SpatialPointsVegsoup")
)
setMethod("SpatialPointsVegsoup",
    signature(obj = "Vegsoup"),
    function (obj) obj@sp.points
)
#if (!isGeneric("SpatialPoints<-")) {
#setGeneric("SpatialPointsVegsoup<-",
#	function (obj, value)
#		standardGeneric("SpatialPointsVegsoup<-")
#)
#}
#setReplaceMethod("SpatialPointsVegsoup",
#	signature(obj = "Vegsoup", value = "SpatialPointsDataFrame"),
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
    signature(obj = "Vegsoup"),
    function (obj) obj@sp.polygons
)
#if (!isGeneric("SpatialPolygons"))
#setGeneric("SpatialPolygonsVegsoup<-",
#	function (obj, value)
#		standardGeneric("SpatialPolygonsVegsoup<-")
#)

#setReplaceMethod("SpatialPolygonsVegsoup",
#	signature(obj = "Vegsoup", value = "SpatialPolygonsDataFrame"),
#	function (obj, value) {
#		#	to do: needs checking of plot names
#		obj@sp.polygons <- value
#		return(obj)		
#	}
#)