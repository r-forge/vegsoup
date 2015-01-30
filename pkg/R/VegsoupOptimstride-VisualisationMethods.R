#	plot method
setMethod("plot",
	signature(x = "VegsoupOptimstride", y = "missing"),
	function (x, mode = 1, oc.threshold = 2, method, retain = FALSE, ...) {

	if (!missing(method)) {
		METHODS <- vegsoup::method(x) # for dispatch to work
		m <- match.arg(method, METHODS, several.ok = TRUE)
		m <- match(m, vegsoup::method(x))
	}
	else {
		m <- 1:length(vegsoup::method(x))
	}	
	
	k <- getK(x)
	ft.threshold <- threshold(x)
	oc1 <- optimclass1(x)
	oc2 <- optimclass2(x)
	p <- peaks(x)
	nm <- nrow(oc1)
	#	there are six default line types
	lty <- rep(1:6, ceiling(nm / 6))[1:nm]
	if (nm > 6)
		col <- rep(c(2,1), each = 6)[1:nm]
	else
		col <- rep(1, nm)

	if (mode == 1) {
		#	open plot
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc1)),
			xlab = "OptimClass1",
			ylab = "No. of significant indicator species",
			sub = paste("Fisher's exact test, threshold",
				format(ft.threshold, scientific = TRUE)), ...)
		#	and add	lines
		for (i in c(1:nm)[m]) {
				lines(1:k, oc1[i, ], lty = lty[i], col = col[i])			
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
			lines(1:k, oc2[i, ], lty = lty[i], col = col[i])
		}
	}
	legend("bottomright",
		lty = 1:length(m),
		legend = vegsoup::method(x)[m], col = col,
		inset = 0.04, bty = "n")
}

)	