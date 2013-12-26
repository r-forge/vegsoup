".elevation" <- function (obj) {
	#	Suggests:
	require(geonames)
	#	cat("request heights for", nrow(obj), "coordinates")
	options(warn = -1)
	res <- unlist(apply(coordinates(obj), 1,
	 function (x) GNsrtm3(lat = x[2], lng = x[1])[1]))
	options(warn = 0)
	if (any(res == -32768)) {
		res[res == -32768] <- 0
	}
	
	if (any(names(obj) == "elevation")) {
		obj$elevation.srtm <- res
	} else {
		obj$elevation <- res
	}
	return(obj)
}

setGeneric("elevation",
	function (obj, ...)
	standardGeneric("elevation")
)

setMethod("elevation",
   signature(obj = "Vegsoup"),
    .elevation
)
