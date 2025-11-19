#if (!isGeneric("extract")) {
	setGeneric("extract", function (x, y, ...)
		standardGeneric("extract"))
#}

setMethod("extract",
	signature(x = "SpatRaster", y = "Vegsoup"), 
	function(x, y, ...){
		terra::extract(x, as(y, "SpatVector"), ...)
	}
)

#	generic imported from terra defines: function(x)
setMethod("ext",
	signature(x = "Vegsoup"), 
	function (x){ 
		b <- bbox(x)
		e <- ext(x = c(b[1,], b[2,]))
		return(e) 
	}
)
