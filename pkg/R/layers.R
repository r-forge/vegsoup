
#	Layers method
".layers.Vegsoup" <- function (obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0, verbose = FALSE) {
	#	return object 
	#	if missing mandatory arguments
	if (missing(collapse) & missing(aggregate)) {
		return(obj@layers)	
	}
	#	if there is nothing to do
	if (length(obj@layers) < 2) {
		message("obj has already only a single layer: ", obj@layers)
		return(obj)
	}
	
	#	check 'aggregate' argument or set defaults
	if (missing(aggregate)) {
		aggregate <- "layer"
	}
	else{
		aggregate <- match.arg(aggregate)	
	}
	
	#	'collapse' missing or of length 1 (full collapse)	
	if (missing(collapse) || length(collapse) == 1) {
		if (verbose) {
			message("collapse to a single layer")
		}
		if (missing(collapse)) {
			collapse <- rep("0l", length(obj@layers)) # use default in doc
		}
		else{
			collapse <- rep(collapse, length(obj@layers))
		}
	}
	else{
		if (length(collapse) > length(obj@layers)) {
			stop("length of collapse vector must match length(Layers(obj))")
		}
	}
	
	#	check 'dec' argument
	
	if (is.ordinal(obj)) { 		
		if (dec == 0 & trunc(min(coverscale(obj)@lims)) == 0) {
			for (dec in 0:5) {
				if (round(min(coverscale(obj)@lims), dec) != 0) break
			}	
			if (verbose) {
				message("set dec to ", dec,
					", min of coverscale(obj)@lims is ",
					min(coverscale(obj)@lims), "!")
			}	
		}
	}
	#	debug
	#	obj = dta; verbose = TRUE; aggregate = "layer"; dec = 0; collapse = "0l"; collapse <- rep(collapse, length(obj@layers))
	#	collapse = c(NA, NA, "sl", "tl", "tl")
	
	res <- obj
	
	X <- species(species(res)) #! get slot data
	Y <- coverscale(res)
	
	collapse <- matrix(c(res@layers, collapse),
		ncol = 2, nrow = length(res@layers),
		byrow = FALSE,
		dimnames = list(NULL, c("original", "collapsed")))
		
	if (verbose) print(collapse)
	
	#	test collapse vector
	if (any(is.na(collapse[,2]))) {
		message("NA in collapse, dropped all species on these layers")
		ld <- collapse[is.na(collapse[,2 ]), 1]
		collapse <- collapse[!is.na(collapse[,2 ]), ]
		#	drop all occurences on these layers
		X <- X[!X$layer %in% ld, ]
		#	also drop from taxonomy
		res@taxonomy <- Taxonomy(res)[Taxonomy(res)$abbr %in% unique(X$abbr), ]
	
		if (length(res@sites$plot) > length(unique(X$plot))) {
			message("also some plots will be dropped")
			res@sites <- res@sites[res@sites$plot %in% X$plot, ]
		}
	}	
	
	X$layer <- factor(X$layer)
	levels(X$layer) <- collapse[match(levels(X$layer), collapse[, 1]), 2]
	X$layer <- as.character(X$layer)
	
	#	convert original abundance scale to numeric to allow calculations
	if (is.ordinal(coverscale(obj))) {
		X$cov <- ordered(X$cov, levels = Y@codes, labels = Y@lims)
		X$cov <- as.numeric(as.character(X$cov))
	}
	else {
		X$cov <- as.numeric(X$cov)
	}
	
	#	aggregate layers
	X  <- switch(aggregate,
	   mean = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = mean)
	}, min = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = min)				
	}, max = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = max)
	}, sum = {
		aggregate(cov ~ plot + abbr + layer, data = X,
			FUN = sum)
	}, layer = {
		if (is.ordinal(coverscale(obj))) {
		aggregate(cov ~ plot + abbr + layer, data = X,
				FUN = function (x) {
					round((1 - prod(1 - x / max(Y@lims))) * max(Y@lims), dec)
				})
		}
		#	critcal! returns 0 for e.g. 0.1 if dec = 0
		else {
			aggregate(cov ~ plot + abbr + layer, data = X,
				FUN = function (x) {
					round((1 - prod(1 - x / 100)) * 100, dec)	
					})
		}		
	})
	
	#	must bring into order
	X <- X[order(X$plot, X$layer, X$abbr), ]
	
	if (!is.null(Y@codes)) {
		if (any(max(X$cov) > max(Y@lims))) {
			message("\nreduced maximum aggregated abundance value to fit into limits: ",
				min(Y@lims)[1], " to ", max(Y@lims))
			X$cov[X$cov > max(Y@lims)] <- max(Y@lims)
		}
	}
	X$cov <- ceiling(X$cov)
	
	#	back convert to original abundance scale if it was character
	if (is.ordinal(coverscale(obj))) {
		X$cov <- as.character(cut(X$cov, breaks = c(0, Y@lims), labels = Y@codes))
	}
	
	res@species <- species(X)
	res@layers <- unique(collapse[, 2])
	#	ensure order
	res@sp.points <- res@sp.points[match(rownames(res), SpatialPointsVegsoup(res)$plot), ]
	res@sp.polygons <- res@sp.polygons[match(rownames(res), SpatialPointsVegsoup(res)$plot), ]
	
	return(invisible(res))
}

setGeneric("Layers",
	function (obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0, verbose = FALSE)
	standardGeneric("Layers")
)
setGeneric("Layers<-", function (obj, value)
	standardGeneric("Layers<-")
)
setMethod("Layers",
   signature(obj = "Vegsoup"),
    .layers.Vegsoup
)
setReplaceMethod("Layers",
	signature(obj = "Vegsoup", value = "ANY"),
	function (obj, value) {
		if (length(value) != length(Layers(obj))) {
			stop("length of value does not match length layers of object")
		}
		if (any(!Layers(obj) %in% value)) {
			stop("items of value do not match layers of object",
				"\n use Layers(obj, collapse = value),",
				" where layers to be dropped are coded as NA") 
		}
		obj@layers <- value
		return(obj)		
	}
)

#	return just the layer columns from species(obj)
setGeneric("Layer",
	function (obj, ...)
		standardGeneric("Layer"))
setMethod("Layer",
   signature(obj = "Vegsoup"),
	function (obj, ...) species(obj)$layer
)
