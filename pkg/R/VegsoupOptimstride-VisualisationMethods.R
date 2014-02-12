#	plot method
setMethod("plot",
	signature(x = "VegsoupOptimstride", y = "missing"),
	function (x, mode = 1, oc.treshold = 2, silent = TRUE, ...) {
	#	require(RColorBrewer)
	#	x <- dta.os
	
	tmp <- summary(object = x, oc.treshold = oc.treshold, silent = silent)
	
	k <- tmp$args$k
	ft.treshold <- tmp$args$ft.treshold
	oc1 <- tmp$optimclass1
	oc2 <- tmp$optimclass2
	cols = 1	

	if (mode == 1) {
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc1)),
			xlab = "OptimClass1",
			ylab = "No. of significant indicator species",
			sub = paste("Fisher's exact test, treshold",
				format(ft.treshold, scientific = TRUE)), ...)
		rug(1:k, side = 3)
		rug(1:k, side = 1)
		axis(3)	
		for (i in 1:nrow(oc1)) {
			lines(1:k, oc1[i, ], lty = i, col = cols)
		}
	}
	if (mode == 2) {
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc2)),
			xlab = "OptimClass2",
			ylab = paste("No. of cluster with more than",
				oc.treshold, "significant indicator species"),
			sub = paste("Fisher's exact test, treshold",
				format(ft.treshold,scientific = TRUE)), ...)
			rug(1:k, side = 3)		
		for (i in 1:nrow(oc2)) {
			lines(1:k, oc2[i, ], lty = i, col = cols)
		}
	}
	legend("bottomright",
		lty = 1:length(tmp$args$method),
		legend = rownames(oc1), col = cols,
		inset = 0.04, bty = "n")
}

)	