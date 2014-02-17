#if (!isGeneric("extract")) {
	setGeneric("extract", function (x, y, ...)
		standardGeneric("extract"))
#}

setMethod("extract",
	signature(x = "Raster", y = "Vegsoup"), 
	function(x, y, ...){
		#	Imports:
		#	require(raster) 
		raster::extract(x, as(y, "SpatialPoints"), ...)
	}
)

#	generic imported from raster defines: function(x, ...)
setMethod("extent",
	signature(x = "Vegsoup"), 
	function (x){ 
		b <- bbox(x)
		e <- new("Extent")
		e@xmin <- b[ 1, 1 ]
		e@xmax <- b[ 1, 2 ]
		e@ymin <- b[ 2, 1 ]
		e@ymax <- b[ 2, 2 ]
		return(e) 
	}
)		