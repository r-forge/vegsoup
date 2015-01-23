setMethod("over",
	signature(x = "Vegsoup", y = "SpatialPolygonsDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...) {
			stopifnot(identicalCRS(x, y))
			p <- SpatialPointsVegsoup(x)
			sites(x) <- cbind(sites(x), over(p, y))
			return(x)
		}
)