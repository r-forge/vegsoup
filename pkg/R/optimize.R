#	optimise partitioning using Dave Roberts OPSTIL procedure
#	optpart defines: function (x, dist, maxitr, ...)
if (!isGeneric("optsil")) {
setGeneric("optsil",
	function (x, maxitr = 100, verbose = FALSE, ...)
		standardGeneric("optsil")
) }

setMethod("optsil",
	signature(x = "VegsoupPartition"),
	function (x, maxitr = 100, verbose = FALSE, ...) {
		k <- getK(x)
		if (k == 1) stop("meaningless with k = ", k)
		
		n <- names(x@part) # save names
		cl <- match.call() # catch call, not used at the moment
		
		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				stop("\n method not defined for R mode", call. = FALSE)
			}
		}
		
		cpu.time <- system.time( {
			r <- optpart::optsil(
					x = partitioning(x), dist = as.dist(x), #, ...
					maxitr = maxitr)
			i <- r$numitr # iterations
		} )
		
		#	modify object
		x@part <- as.integer(r$clustering)
		names(x@part) <- n
		
		#	test if we have to change slot k
		kk <- length(unique(partitioning(x)))
		if (k != kk) {
			x@k <- kk
			#	good language
			message(ifelse(k > kk, "decreased ", "increased "),
				"number of partitons from ", k, " to ", kk)
		}
		
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(x), "cells",
				"and", k, "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", i)
		}
		return(x)
	}
)

#	optimise partitioning using Dave Roberts OPTINDVAL procedure
#	optpart defines: function (veg, clustering, maxitr = 100, minsiz = 5, ...)
if (!isGeneric("optindval")) {
setGeneric("optindval",
	function (x, maxitr = 100, minsiz = 5, verbose = FALSE, ...)
		standardGeneric("optindval")
) }
setMethod("optindval",
	signature(x = "VegsoupPartition"),
	function (x, maxitr = 100, minsiz = 5, verbose = FALSE, ...) {
		k <- getK(x)
		if (k == 1) stop("meaningless with k = ", k)
		
		n <- names(x@part) # save names
		cl <- match.call() # catch call, not used at the moment
		
		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				stop("\n method not defined for R mode", call. = FALSE)
			}
		}
		
		cpu.time <- system.time( {
			r <- optpart::optindval(
					as.matrix(x, ...), partitioning(x),
					maxitr = maxitr,
					minsiz = minsiz)
			i <- r$numitr # iterations
		} )
		
		#	modify object
		x@part <- as.integer(r$clustering)
		names(x@part) <- n
		
		#	test if we have to change slot k
		kk <- length(unique(partitioning(x)))
		if (k != kk) {
			x@k <- kk
			#	good language
			message(ifelse(k > kk, "decreased ", "increased "),
				"number of partitons from ", k, " to ", kk)
		}
		
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(x), "cells",
				"and", k, "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", i)
		}
		return(x)
	}
)

#	optimise partitioning using Attila Lengyel's REMOS method
#	code below as provided in electronic supplement for the paper
#	Lengyel, A., Roberts, D.W., Botta-Dukát, Z. Comparison of silhouette-based
#	reallocation methods for vegetation classification. Journal of Vegetation
#	Science. Appendix S1 – R code for the REMOS algorithms
#	renamed arguments symbols
#	max.iter > maxitr as in optsil and optpart, SIl > S, #	neig > neighbor
#	gr_new > G, parts > k, fin1 > r1, fin2 > r2, gr_final > r, result > r

.remos <- function(d, gr, lim = -0.001, method = c(1,2), maxitr = Inf)  {

	#	test method argument
	METHOD <- c("1", "2")
	method <- match.arg(toupper(method), METHOD)

	G <- gr
	o <- -1
	z <- 1
	dw <- 1
	w <- u <- y <- vector("numeric")
	W <- matrix(NA, nrow = length(gr), ncol = 0)
	parts <- vector("numeric")
	
	while (o < lim) {
		S <- cluster::silhouette(G, d)
		#	clustering <- S[ , 1]
		neighbor <- S[ , 2]
		width <- S[ , 3]
		W <- cbind(W, width)
		if (method == 1) {
			worst <- which.min(width)
		}
		if (method == 2) {
			worst <- which(width < lim)
		}
		if (z > 1) {
			dw <- as.vector(colSums( (W - width)^2 )[ -z ])
		}
		o <- min(width)
		if (o < lim) {
			G[ worst ] <- neighbor[ worst ]
		}
		y[ z ]<- o
		u[ z ]<- sum(width < 0)
		w[ z ]<- abs(sum(width[ width < 0 ]))
		parts <- cbind(parts, G)
		if (any(dw == 0)) { o <- lim + 1 }
		if (z == maxitr) { o <- lim + 1 }
		z <- z + 1
	}
	
	r1 <- u == min(u)
	r2 <- w == min(w[ r1 ]) & r1
	r <- parts[, r2 ]
	
	#	return.partitions == TRUE
	r <- list(clustering = r, parts, y, u, w)
	
	return(r)
}

if (!isGeneric("remos")) {
setGeneric("remos",
	function (x, lim = -0.001, method = 2, maxitr = Inf, verbose = FALSE, ...)
		standardGeneric("remos")
) }

setMethod("remos",
	signature(x = "VegsoupPartition"),
	function (x, lim = -0.001, method = 2, maxitr = Inf, verbose = FALSE, ...) {
		k <- getK(x)
		if (k == 1) stop("meaningless with k = ", k)
		
		n <- names(x@part) # save names
		cl <- match.call() # catch call, not used at the moment

		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				stop("\n method not defined for R mode", call. = FALSE)
			}
		}
		
		cpu.time <- system.time( {
			r <- .remos(
					d = as.dist(x), gr = partitioning(x),
					#as.dist(x, ...), partitioning(x),
					lim = lim,
					method = method,
					maxitr = maxitr)
			i <- r$numitr # iterations
		} )
		
		#	modify object
		x@part <- as.integer(r$clustering)
		names(x@part) <- n
		
		#	test if we have to change slot k
		kk <- length(unique(partitioning(x)))
		if (k != kk) {
			x@k <- kk
			#	good language
			message(ifelse(k > kk, "decreased ", "increased "),
				"number of partitons from ", k, " to ", kk)
		}
		
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(x), "cells",
				"and", k, "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", i)
		}
		return(x)
	}
)
