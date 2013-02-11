
#	Layers method
".layers.Vegsoup" <- function (obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0, verbose = FALSE) {
if (missing(collapse) & missing(aggregate)) {
	return(obj@layers)	
} else {
	if (length(obj@layers) < 2) {
		if (verbose) warning("\n obj has already only a single layer: ", obj@layers, call. = FALSE)
		return(obj)
	} else {
	
	#	check supplied arguments	
	if (missing(aggregate)) {
		aggregate <- "layer"
	} else {
		aggregate <- match.arg(aggregate)	
	}
	if (missing(collapse) || length(collapse) == 1) {
		if (verbose) {
			cat("collapse to a single layer\n")
		}
		if (missing(collapse)) {
			collapse <- rep("0l", length(obj@layers))
		} else {
			collapse <- rep(collapse, length(obj@layers))
		}
	} else {
		if (length(collapse) > length(obj@layers)) {
			stop("length of collapse vector must match length(Layers(obj))")
		}
	}
	
	#	debug
	#	obj = dta; verbose = TRUE; aggregate = "layer"; dec = 0; collapse = c(NA, NA, "sl", "tl", "tl")
	
	#	revert to class Vegsoup and cast again
	#	not needed any more
	#if (inherits(obj, "Vegsoup")) {
	#	res <- as(obj, "Vegsoup")
	#} else {
		res <- obj
	#}

	species <- Species(res)
	scale <- coverscale(res)

	collapse <- matrix(c(res@layers, collapse),
		ncol = 2, nrow = length(res@layers),
		byrow = FALSE,
		dimnames = list(NULL, c("original", "collapsed")))
		
	if (verbose) {
		print(collapse)
	}
	
	if (any(is.na(collapse[,2]))) {
		warning("NA in collapse, all species on these layers will be dropped", call. = FALSE)
		ld <- collapse[is.na(collapse[,2 ]), 1]
		collapse <- collapse[!is.na(collapse[,2 ]), ]
		#	drop all occurences on these layers
		species <- species[!species$layer %in% ld, ]
		#	also drop from taxonomy
		res@taxonomy <- Taxonomy(res)[Taxonomy(res)$abbr %in% unique(species$abbr), ]

		if (length(res@sites$plot) > length(unique(species$plot))) {
			warning("also some plots will be dropped", call. = FALSE)
			res@sites <- res@sites[res@sites$plot %in% species$plot, ]
		}		
		
		#	warning! also need to subset spatial objects!
	}	

	species$layer <- factor(species$layer)
	levels(species$layer) <- collapse[match(levels(species$layer), collapse[, 1]), 2]
	species$layer <- as.character(species$layer)

	scale.is.character <- is.character(species$cov)
	
	#	convert original abundance scale to numeric to allow calculations
	if (scale.is.character) {
		species$cov <- as.factor(species$cov)
		levels(species$cov) <- scale@lims[match(levels(species$cov), scale@codes)]
		species$cov <- as.numeric(as.character(species$cov))
	}
	
	#	aggregate layers
	species  <- switch(aggregate, mean = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = mean)
	}, min = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = min)				
	}, max = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = max)
	}, sum = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = sum)
	}, layer = {
		if (!is.null(scale@codes)) {
		aggregate(cov ~ plot + abbr + layer, data = species,
				FUN = function (x) {
					round((1 - prod(1 - x / max(scale@lims))) * max(scale@lims), dec)
				})
		} else {
			aggregate(cov ~ plot + abbr + layer, data = species,
				FUN = function (x) {
					round((1 - prod(1 - x / 100)) * 100, dec)	
					})
		}		
	})
	
	species <- species[order(species$plot, species$layer, species$abbr), ]
	
	if (!is.null(scale@codes)) {
		if (any(max(species$cov) > max(scale@lims))) {
			warning("\n reduced maximum aggregated abundance value to fit into limits: ",
				min(scale@lims)[1], " to ", max(scale@lims), call. = FALSE)
			species$cov[species$cov >  max(scale@lims)] <- max(scale@lims)
		}
	}
	species$cov <- ceiling(species$cov)
	
	#	back convert to original abundance scale if it was character
	if (scale.is.character) {
		species$cov <- as.character(cut(species$cov, breaks = c(0, scale@lims), labels = scale@codes))
	}
	
	res@species <- species
	res@layers <- unique(collapse[, 2])
	#res <- Vegsoup(res)
	
	return(invisible(res))

	}
	}
}

setGeneric("Layers",
	function (obj, ...)
	standardGeneric("Layers")
)
setGeneric("Layers<-", function (obj, value)
	standardGeneric("Layers<-")
)
setMethod("Layers",
   signature(obj = "Vegsoup"),
    .layers.Vegsoup
)
#	return just the layer columns from Species(obj)
setGeneric("Layer",
	function (obj, ...)
		standardGeneric("Layer"))
setMethod("Layer",
   signature(obj = "Vegsoup"),
	function (obj, ...) Species(obj)$layer
)
