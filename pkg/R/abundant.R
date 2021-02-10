setGeneric("abundant",
	function (obj, threshold = 75, layers = FALSE, ...)
		standardGeneric("abundant")
)

setMethod("abundant",
	signature(obj = "Vegsoup"),
	function (obj, threshold = 75, layers = FALSE, ...) {
		if (inherits(obj, "VegsoupPartition") | inherits(obj, "VegsoupPartitionFidelity"))	{
			obj <- as(obj, "Vegsoup")	
		}
		if (layers) {
			r <- obj
		} else {
			r <- layers(obj, "0l")
		}
		
		decostand(r) <- NULL
		
		r1 <- constancy(r)
		r2 <- quantile(r, coverscale = TRUE)[ , , 3]
		r3 <- quantile(r, coverscale = FALSE)[ , , 3]
	
		r <- as.data.frame(r1)
		ri <- decode(r, obj)

		r$labels <- r2
		r$cover <- r3
		r$taxon <- ri$taxon
		r$layer <- ri$layer		
		names(r)[ 1 ] <- "constancy"
		r <- r[ r$constancy >= threshold, ]

		if (layers) {
			r <- r[ order(r$constancy, r$layer, r$cover, r$taxon, decreasing = TRUE), ]
		} else {
			r <- r[ order(r$constancy, r$cover, r$taxon, decreasing = TRUE), ]
		}

		return(r)
	}
)