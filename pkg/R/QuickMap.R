#	inherited visulalisation method
#if (!isGeneric("gvisMap")) {
setGeneric("QuickMap",
	function (x)
		standardGeneric("QuickMap")
)
#}

#	gvisMap package
setMethod("QuickMap",
    signature(x = "Vegsoup"),
    function (x) {
    	suppressPackageStartupMessages(require(googleVis))
		pt <- SpatialPointsVegsoup(x)
		if (nrow(pt) > 1) {
			df <- data.frame(LatLong = apply(coordinates(pt)[, c(2,1)], 1,
				function (x) paste(x, collapse = ":")),
			Tip = pt$plot)
		} else {
			df <- data.frame(LatLong = paste(coordinates(pt)[, c(2,1)], collapse = ":"),
			Tip = pt$plot)
		}
		m <- gvisMap(df, "LatLong" , "Tip")
		plot(m)
		return(invisible(m))
	}
)