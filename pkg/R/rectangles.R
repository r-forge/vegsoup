#	alternative plotting methof
#	not a generic for plot

if (!isGeneric("rectangles")) {
setGeneric("rectangles",
	function (obj, plot = TRUE, ...)
	standardGeneric("rectangles"))
}

setMethod("rectangles",
	signature(obj = "VegsoupPartition"),
	function (obj, plot = TRUE, ...) {
	
	if (missing(plot)) {
		plot = TRUE	
	}
	p <- unique(partitioning(obj))
	d <- dim(obj)
	
	#	subsetting will issue a warning
	#	this is harmless and not of interessent at this point
	op <- options()
	options(warn = -1)
	res <- t(sapply(sort(p), function(x) {
		dim(obj[partitioning(obj) == x, ])
	}))
	options(op)
	
	res <- cbind(res, p)
	if (plot) {
		#	order
		r <- res[order(res[, 1], res[,2]), ]
		plot(max(r[, 1]), max(r[, 2]),
			xlim = c(0, max(r[, 1])), ylim = c(0, max(r[, 2])),
			type = "n", bty = "n",
			xlab = "number of sites", ylab = "number of species",
			sub = paste("total number of sites and species:",
				d[1], d[2]
			))
		rect(0, 0, r[,1], r[, 2], ...)
		points(r[,1] , r[,2], pch = 16, col =" white", cex = 3)
		text(r[,1] , r[,2], labels = r[, 3], cex = 1, font = 2)
		rug(1, side = 1, lwd = 1)
	}
	
	return(res)
	}
)