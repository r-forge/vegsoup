
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
#	debug
#	obj = dta; verbose = TRUE; aggregate = "layer"; dec = 0; collapse = "0l"; collapse <- rep(collapse, length(obj@layers))
#	collapse = c(NA, NA, "sl", "tl", "tl")

res <- obj

species <- Species(res)
scale <- coverscale(res)

collapse <- matrix(c(res@layers, collapse),
	ncol = 2, nrow = length(res@layers),
	byrow = FALSE,
	dimnames = list(NULL, c("original", "collapsed")))
	
if (verbose) print(collapse)

#	test collapse vector
if (any(is.na(collapse[,2]))) {
	message("\nNA in collapse, dropped all species on these layers")
	ld <- collapse[is.na(collapse[,2 ]), 1]
	collapse <- collapse[!is.na(collapse[,2 ]), ]
	#	drop all occurences on these layers
	species <- species[!species$layer %in% ld, ]
	#	also drop from taxonomy
	res@taxonomy <- Taxonomy(res)[Taxonomy(res)$abbr %in% unique(species$abbr), ]

	if (length(res@sites$plot) > length(unique(species$plot))) {
		message("also some plots will be dropped")
		res@sites <- res@sites[res@sites$plot %in% species$plot, ]
	}
}	

species$layer <- factor(species$layer)
levels(species$layer) <- collapse[match(levels(species$layer), collapse[, 1]), 2]
species$layer <- as.character(species$layer)

#	convert original abundance scale to numeric to allow calculations
if (is.ordinal(coverscale(obj))) {
	species$cov <- ordered(species$cov, levels = scale@codes, labels = scale@lims)
	species$cov <- as.numeric(as.character(species$cov))
	
#	species$cov <- as.factor(species$cov)
#	levels(species$cov) <- scale@lims[match(levels(species$cov), scale@codes)]
#	species$cov <- as.numeric(as.character(species$cov))
} else {
	species$cov <- as.numeric(species$cov)
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
	if (is.ordinal(coverscale(obj))) {
	aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = function (x) {
				round((1 - prod(1 - x / max(scale@lims))) * max(scale@lims), dec)
			})
	}
	#	critcal! returns 0 for e.g. 0.1 if dec = 0
	else{
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = function (x) {
				round((1 - prod(1 - x / 100)) * 100, dec)	
				})
	}		
})

#	must bring into order
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
if (is.ordinal(coverscale(obj))) {
	species$cov <- as.character(cut(species$cov, breaks = c(0, scale@lims), labels = scale@codes))
}

res@species <- species
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

#	return just the layer columns from Species(obj)
setGeneric("Layer",
	function (obj, ...)
		standardGeneric("Layer"))
setMethod("Layer",
   signature(obj = "Vegsoup"),
	function (obj, ...) Species(obj)$layer
)
