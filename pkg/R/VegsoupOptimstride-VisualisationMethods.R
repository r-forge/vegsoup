#	plot method
setMethod("plot",
	signature(x = "VegsoupOptimstride", y = "missing"),
	function (x, mode = 1, oc.threshold = 2, method, retain = FALSE, ...) {
	#	require(RColorBrewer)

	if (!missing(method)) {
		METHODS <- methods(x)
		m <- match.arg(method, METHODS, several.ok = TRUE)
		m <- match(m, methods(x))
	}
	else {
		m <- 1:length(methods(x))
	}	
	
	k <- getK(x)	
	ft.threshold <- threshold(x)
	oc1 <- optimclass1(x)
	oc2 <- optimclass2(x)
	p <- peaks(x)
	cols = 1	

	if (mode == 1) {
		#	open plot
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc1)),
			xlab = "OptimClass1",
			ylab = "No. of significant indicator species",
			sub = paste("Fisher's exact test, threshold",
				format(ft.threshold, scientific = TRUE)), ...)
		#	and add	lines
		for (i in c(1:nrow(oc1))[m]) {
				lines(1:k, oc1[i, ], lty = i, col = cols)			
				rug(p[[i]], side = 3, lwd = 5, col = "grey80", ticksize = -0.03)
		}			
		#	add rug axes to help eye-balling curve peaks
		rug(1:k, side = 3)
		rug(1:k, side = 1)
		axis(3)		
	}
	if (mode == 2) {
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc2)),
			xlab = "OptimClass2",
			ylab = paste("No. of cluster with more than",
				oc.threshold, "significant indicator species"),
			sub = paste("Fisher's exact test, threshold",
				format(ft.threshold,scientific = TRUE)), ...)
			rug(1:k, side = 3)		
		for (i in 1:nrow(oc2)) {
			lines(1:k, oc2[i, ], lty = i, col = cols)
		}
	}
	legend("bottomright",
		lty = 1:length(m),
		legend = methods(x)[m], col = cols,
		inset = 0.04, bty = "n")
}

)	