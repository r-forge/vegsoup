#	generating function
#setMethod("print",
#    signature(object = "VegsoupOptimstride"),
#    function (object) {
#			print(object@optimstride)
#    }
#)

setMethod("show",
    signature(object = "VegsoupOptimstride"),
    function (object) {
			summary(object)
    }
)
setMethod("summary",
	signature(object = "VegsoupOptimstride"),
		function (object, oc.treshold = 2, silent = FALSE) {
			#	object <- opt; oc.treshold = 2
		obj <- object@optimstride
		args <- obj$settings$args
		met <- args$method
		ind <- obj$indicators
		ftt <- args$ft.treshold
		oct <- oc.treshold
		args$oc.treshold <- oct

		oc1 <- t(sapply(ind, function (x) sapply(x, function (x) sum(x))))
		oc2 <- t(sapply(ind, function (x) sapply(x, function (x) length(which(x >= oct)))))
		
		#	not a good guess!
		best.oc1 <- sapply(obj$ind, function (x) (sapply(x, max)[which.max(sapply(x, max))] ) )

		res <- list(optimclass1 = oc1, optimclass2 = oc2, best.optimclass1 = best.oc1, args = args)
	
		if (!silent) {
			cat("OptimStride results for k:", args$k)
			cat("\n\nOptimClass 1 (fisher test treshold: ", ftt, "):\n", sep = "")
			print(res$optimclass1)
	
			cat("\nOptimClass 2 (occurence treshold: ",
				oct, "):\n", sep = "")
			print(res$optimclass2)
			
			cat("\nBest OptimClass\n", sep = "")
			print(res$best.optimclass1)
		}
		return(invisible(res))
		}
)

#if (!isGeneric("plot")) {
#	setGeneric("plot", function(x, y, ...)
#		standardGeneric("plot"))
#}	

#	plot method
setMethod("plot",
	signature(x = "VegsoupOptimstride", y = "missing"),
	function (x, mode = 1, oc.treshold = 2, silent = TRUE, ...) {
	#	x <- dta.os
	
	tmp <- summary(object = x, oc.treshold = oc.treshold, silent = silent)
	
	k <- tmp$args$k
	ft.treshold <- tmp$args$ft.treshold
	oc1 <- tmp$optimclass1
	oc2 <- tmp$optimclass2

	if (mode == 1) {
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc1)),
			xlab = "OptimClass1",
			ylab = "No. of significant indicator species",
			sub = paste("Fisher's exact test, treshold",
				format(ft.treshold, scientific = TRUE)), ...)
			rug(1:k, side = 3)
		for (i in 1:nrow(oc1)) {
			lines(1:k, oc1[i, ], lty = i)
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
			lines(1:k, oc2[i, ], lty = i)
		}
	}
	legend("bottomright",
		lty = 1:length(tmp$args$method),
		legend = rownames(oc1), bty = "n")
}

)	