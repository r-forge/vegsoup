setMethod("over",
	signature(x = "Vegsoup", y = "SpatialPolygonsDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...) {
			p <- SpatialPointsVegsoup(x)
			stopifnot(sp::identicalCRS(p, y))
			sites(x) <- cbind(sites(x), sp::over(p, y))
			return(x)
		}
)