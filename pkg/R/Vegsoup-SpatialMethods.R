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

#	taken from SpatialPointsDataFrame-methods in package sp
setReplaceMethod("coordinates",
	signature(object = "Vegsoup", value = "ANY"),
	function (object, value) {
		#	only formula method
		if (!inherits(value, "formula")) {
			stop("only formula method is implemented (e.g. coordinates(x) <- ~X+Y", call. = FALSE)
		}	
		xy <- model.frame(value, Sites(object))
		if (dim(xy)[2] == 2) {
			jj <- as.character(as.list(value)[[2]])[2:3]
			j <- match(jj, names(object))
		} else if (dim(xy)[2] == 3) { 
			jj <- c(as.character(as.list((as.list(value)[[2]])[2])[[1]])[2:3],
				as.character(as.list(value)[[2]])[3])
			j <- match(j, names(object))
		}		
		object@sp.points <- SpatialPointsDataFrame(
			coords = xy, data = slot(slot(object, "sp.points"), "data"),
			match.ID = FALSE)
		Sites(object) <- Sites(object)[, -j]
		return(object)			
	}
)
		
#	proj4string method
setMethod("proj4string",
	signature(obj = "Vegsoup"),
	function (obj) proj4string(obj@sp.points)
)

#setReplaceMethod("proj4string",
#	signature(obj = "Vegsoup", value = "character"),
#	function (obj, value) {
#		value <- CRS(value)
#		proj4string(obj@sp.points) <- value
#		obj
#	}
#)

setReplaceMethod("proj4string",
	signature(obj = "Vegsoup", value = "CRS"),
	function (obj, value) {
		proj4string(obj@sp.points) <- value
		obj
	}
)

setMethod("spTransform",
	signature(x = "Vegsoup", "CRS"),
	function (x, CRSobj, ...) {
		#	Suggests:
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
    signature(obj = "Vegsoup"),
    function (obj) {
    	obj@sp.points
    }
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

#	dispatch on Spatial*
setAs(from = "Vegsoup", to = "SpatialPoints",
	def = function (from) {
		as(from@sp.points, "SpatialPoints")
	}
)

#as.SpatialPoints.Vegsoup <- function (x) {
#	as(x, "SpatialPoints")
#}	

setAs(from = "Vegsoup", to = "SpatialPointsDataFrame",
	def = function (from) {
		from@sp.points
	}
)

#as.SpatialPointsDataFrame.Vegsoup <- function (x) {
#	as(x, "SpatialPointsDataFrame")
#}	

#	plot spatial points
points.Vegsoup <- function (x, y = NULL, ...) {
	points(coordinates(x), ...)
}