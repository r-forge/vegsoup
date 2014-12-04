setMethod("over",
	signature(x = "Vegsoup", y = "SpatialPolygonsDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...) {
			stopifnot(identicalCRS(x, y))
			p <- SpatialPointsVegsoup(x)
			Sites(x) <- cbind(Sites(x), over(p, y))
			return(x)
		}
)