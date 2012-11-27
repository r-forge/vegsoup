#	function to cast species matrix
#	mode = 2 binary
#	mode = 1 charcter, or numeric
.cast <- function (obj, mode, ...) {
	#	obj = dta; mode = 2
			
#	cpu.time <- system.time({

	#	slots
	plot <- slot(obj, "species.long")$plot
	abbr <- slot(obj, "species.long")$abbr
	layer <- slot(obj, "species.long")$layer
	cov <- slot(obj, "species.long")$cov
	scale <- slot(obj, "scale")
	
	#	matrix dimensions
	plots <- unique(plot)
	species.layer <- paste(abbr, layer, sep = "@")

	#	resort to layer
	if (length(Layers(obj)) > 1) {	
	#	slow, but ensures order
		species <- unique(as.vector(unlist(
			sapply(Layers(obj),
				function (x) {
					species.layer[layer == x]
				}
			))))
	} else {
	#	fast	
		species <- unique(species.layer)	
	}
			
	#	cover transformation
	if (mode == 1 & scale$scale != "frequency") {
		cov <- factor(cov, levels = scale$codes, labels = scale$lims)
		if (any(is.na(cov))) stop("scale codes do not perfectly match data" )
	}
#	})
#	cat("\n time to init objects", cpu.time[3], "sec")
	if (mode == 1) {
		cpu.time <- system.time({	
		m <- t(vapply(plots,
			USE.NAMES = FALSE,
			FUN.VALUE = numeric(length(species)),
			FUN = function (x) {
				r <- numeric(length(species))
				r[match(species.layer[plot == x], species)] <- cov[plot == x]
				#	was r[species %in% species.layer[plot == x]] <- cov[plot == x]
				r
			}))
		dimnames(m) <- list(plots, species)		
		})
	}

	if (mode == 2) {
		cpu.time <- system.time({	
		m <- t(vapply(plots,
			USE.NAMES = FALSE,
			FUN.VALUE = character(length(species)),
			FUN = function (x) {
				r <- character(length(species))
				#	change to ""
				#	there are several function that look for 0!
				r[] <- "0"
				r[match(species.layer[plot == x], species)] <- cov[plot == x]
				#	was	r[species %in% species.layer[plot == x]] <- cov[plot == x]
				r
			}))
		dimnames(m) <- list(plots, species)		
		})
	}
		
	if (mode == 3) {
		cpu.time <- system.time({			
		m <- t(vapply(plots,
			USE.NAMES = FALSE,
			FUN.VALUE = integer(length(species)),
			FUN = function (x) {
				r <- integer(length(species))
				r[species %in% species.layer[plot == x]] <- as.integer(1)
				r
			}))
		dimnames(m) <- list(plots, species)
		})		
	}
	#	cat("\n time to cast matrix", cpu.time[3], "sec")		

	return(invisible(m))
}

#	generating function
#	to do: documentation, high priority!

VegsoupData <- function (obj, verbose = FALSE) {
	require(stats)
	#	obj <- qry; verbose = TRUE	
	if (!inherits(obj, "Vegsoup")) {
		stop("Need object of class Vegsoup")
	}
	
	scale <- AbundanceScale(obj)
	lay <- Layers(obj)
	txa <- Taxonomy(obj)
	species.long <- SpeciesLong(obj)
	
	#	 cats species matrix for Braun-Blanquet scales	
	if (scale$scale == "Braun-Blanquet" | scale$scale == "Braun-Blanquet 2") {
		if (!is.character(species.long$cov)) {
			stop("Abundance scale should be of mode charcter")
		}
		if (length(lay) == 1) {
			if (verbose) cat("\ndata is structered in only one layer")
		} else {
			if (verbose) cat("\ndata is structered in layers: ", lay)
		}
		
		species <- as.data.frame(.cast(obj, mode = 2), stringsAsFactors = FALSE)
			
	} # end if "Braun Blanquet" | "Braun-Blanquet 2"

	#	cast species matrix for Domin scale
	#	this is Braun-Blanquet scale
	
	#	rewrite to use .cast() 
	if (scale$scale == "Domin") {
		if (!is.character(species.long$cov)) {
			if (is.integer(species.long$cov)) {
				warning("\n Codes for Domin scale supplied as integer, ",
					"attempt to change mode to charcter", call. = FALSE)
				species.long$cov <- as.character(species.long$cov)
			} else {
				stop("Abundance code must be either of mode charcter ",
					"or integer for Domin scale")
			}
		}
		if (length(lay) == 1) {
			if (verbose) cat("\ndata is structered in only one layer")
		} else {
			if (verbose) cat("\ndata is structered in layers: ", lay)
		}
		
		species <- .cast(obj, mode = 2)
		species <- as.data.frame(res, stringsAsFactors = FALSE)
	} # end if "Domin"
	
	#	rewrite to use .cast() 
			
	cpu.time <- system.time({	
	if (scale$scale == "frequency" | scale$scale == "binary") {

		if (!is.numeric(species.long$cov)) {
			#	warning("changed mode to numeric", str(species.long$cov))
			mode(species.long$cov) <- "numeric"
		}	
		xt  <- xtabs(cov ~ plot + abbr + layer,
			data = species.long)
	
		if (dim(xt)[3] > 1) {
			res <- matrix(0,
			ncol = dim(xt)[2] * dim(xt)[3],
			nrow = dim(xt)[1],
			dimnames = list(
				plot = dimnames(xt)$plot, 
				abbr = paste(rep(dimnames(xt)$abbr, dim(xt)[3]),
					rep(dimnames(xt)$layer,
					each = dim(xt)[2]), sep = "@")))
		} else {
			res <- matrix(0,
			ncol = dim(xt)[2],
			nrow = dim(xt)[1],
			dimnames = list(
				plot = dimnames(xt)$plot, 
				abbr = paste(dimnames(xt)$abbr,
						dimnames(xt)$layer, sep = "@")))
		}
		for (i in 1:dim(xt)[3]) {
			sel <- grep(paste("", dimnames(xt)$layer[i], sep = "@"),
				dimnames(res)$abbr, fixed = TRUE)
			res[,sel] <- xt[,,i]	
		}
		res <- res[, colSums(res) > 0]
		species <- as.data.frame(res)
	} # end if "frequency"	
	}) # end system.time
	
	if (verbose) {
		cat("\ntime to cast species matrix",
		"of", prod(dim(res)), "cells:",
		cpu.time[3], "sec\n")
	}
	
	#	develop class VegsoupData from class Vegsoup
	res <- new("VegsoupData", obj)
	#	assign class slot
	res@species = species
#	res@sites = sites			

	return(res)
}

### inherited methods based on 'generic functions'
#	as.character and as.numeric are already generic functions
#if(!isGeneric("as.binary"))
setGeneric("as.binary",
	function (x, ...)
		standardGeneric("as.binary")
)
#}

#	print mode uses invisible()?
#	use e.g. head(as.numeric(obj))?
setMethod("as.character",
    signature(x = "VegsoupData"),
    function (x) {
    	return(invisible(.cast(x, mode = 2)))
    }
)

setMethod("as.numeric",
    signature(x = "VegsoupData"),
    function (x) {
    	return(invisible(.cast(x, mode = 1)))		    
    }
)
	
setMethod("as.binary",
    signature(x = "VegsoupData"),
    function (x) {
    	return(invisible(.cast(x, mode = 3)))	    
    }
)	

#	'names' is a primitive function
#	rename to colnames for consitency
setMethod("names",
    signature(x = "VegsoupData"),
    function (x) {
	#	adapted from .cast()
	abbr <- slot(x, "species.long")$abbr
	layer <- slot(x, "species.long")$layer
	species.layer <- paste(abbr, layer, sep = "@")
	res <- unique(as.vector(unlist(
			sapply(Layers(x),
				function (x) {
					species.layer[layer == x]
				}
			))))
	#	was
	#	unique(paste(
	#		slot(x, "species.long")$abbr,
	#		slot(x, "species.long")$layer, sep = "@"))
	}
)


###	inherited
#	methods based on functions in 'base'
#if (!isGeneric("rownames")) {
setGeneric("rownames", function (x, do.NULL = TRUE, prefix = "row")
	standardGeneric("rownames"))
#}	
setMethod("rownames",
    signature(x = "VegsoupData", do.NULL = "missing", prefix = "missing"),
    function (x) {
		unique(slot(x, "species.long")$plot)	
		#	was rownames(x@species)
	}
)

#	'dim' is a primitive function
setMethod("dim",
    signature(x = "VegsoupData"),
	    function (x) {
			c(nrow(x), ncol(x))
		#	was dim(x@species)
		}
)

#if (!isGeneric("nrow")) {
setGeneric("nrow", function(x)
	standardGeneric("nrow"))
#}
#	to do: documentation
setMethod("nrow",
    signature(x = "VegsoupData"),
    function (x) {
		length(rownames(x))
	#	was nrow(x@species)
	}
)
#if (!isGeneric("dim")) {
setGeneric("ncol", function(x)
	standardGeneric("ncol"))
#}
#	to do: documentation
setMethod("ncol",
    signature(x = "VegsoupData"),
    function (x) {
		length(names(x))
	#	was ncol(x@species)
	}
)
#if (!isGeneric("rowSums")) {
setGeneric("rowSums", function (x, na.rm = FALSE, dims = 1)
	standardGeneric("rowSums"))
#}
#	to do: documentation
setMethod("rowSums",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1) {
    	rowSums(as.binary(x))
    }
)
#if (!isGeneric("colSums")) {
setGeneric("colSums", function (x, na.rm = FALSE, dims = 1)
	standardGeneric("colSums"))
#}
#	to do: documentation
setMethod("colSums",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1) {
    	colSums(as.binary(x))
    }
)

#	inherited methods based on functions in 'utils'
#if (!isGeneric("head")) {
setGeneric("head", function(x)
	standardGeneric("head"))
#}
#	to do: documentation
setMethod("head",
    signature(x = "VegsoupData"),
    function (x, n = 6L, choice, mode, ...) {
	    if (missing(choice))
	    	choice = "species"	
	    if (missing(mode))
		    mode = 3 # binary
	    if (missing(n))
		    n = 6L
    	if (choice == "species")
			res <- head(as.binary(x), n, ...)
    		#	was res <- head(x@species)
    	if (choice == "sites")
    		res <- head(Sites(x), n, ...)
    	return(res)
    }    	    
)
#if (!isGeneric("tail")) {
setGeneric("tail", function (x)
	standardGeneric("tail"))
#}
#	to do: documentation
setMethod("tail",
    signature(x = "VegsoupData"),
    function (x, n = 6L, choice, mode, ...) {
	    if (missing(choice))
	    	choice ="species"
		if (missing(mode))
		    mode = 3 # binary
	    if (missing(n))
			n = 6L
		if (choice == "species")
			res <- tail(as.binary(x), n, ...)
    		#	res <- tail(x@species, ...)
    	if (choice == "sites")
    		res <- tail(Sites(x), n, ...)
    	return(res)
    }    	    
)

###	inherited methods from 'Vegsoup'
#	getter method for sites
#	to do: documenation

#	coercion method
#	coercion to class Vegsoup is automatic as defined by the contains= argument
#	to do: documenation
setAs("VegsoupData", "list",
	def = function (from) {
		list(
		species = as.character(from)@species,
#	was	species = from@species,
		sites = from@sites)
	}
)

#if (!isGeneric("Sites")) {
setGeneric("Sites",
	function (obj, ...)
		standardGeneric("Sites")
)
#}
setMethod("Sites",
    signature(obj = "VegsoupData"),
    function (obj) obj@sites
)
#if (!isGeneric("Sites<-")) {
setGeneric("Sites<-",
	function (obj, value, ...)
		standardGeneric("Sites<-")
)
#}
#	replacement method for sites
#	to do: needs comprehensive validity checks!
setReplaceMethod("Sites",
	signature(obj = "VegsoupData", value = "data.frame"),
	function (obj, value) {
		obj@sites <- value		
		return(obj)		
	}
)

#	indexing method
setMethod("$", "VegsoupData", 
	function(x, name) {
		if (!("sites" %in% slotNames(x)))
			stop("no $ method for object without slot sites")
		x@sites[[name]]
		#do.call("$", list = (Sites(x), name))
	}
)

#	subsetting method
#	to do: documenation
#	VegsoupDataPartition implemts its own method,
#	but internally coreces to class(VegsoupData)
#	and applies this method!
setMethod("[",
    signature(x = "VegsoupData",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = TRUE)
    {
	    #	debug
	    #	x = dta; i = 1; j <- c(4,7,9,1,12); j <- rep(TRUE, ncol(x))
		#	x <- prt; i = Partitioning(x) == 2
	    res <- x
	    if (missing(i)) i <- rep(TRUE, nrow(res))
	    if (missing(j)) j <- rep(TRUE, ncol(res))
	    #	change to as.binary(x)[i, j, ...]
		#	when slot species is dropped
		tmp <- as.character(x)[i, j, ...]
		#	validity
		if (all(unlist(tmp) == 0)) {
			stop(call. = FALSE, "subset does not contain any species!")
		}
		#	if single plot "[" method will return class character
		if (class(tmp) == "character") {
			tmp <- t(matrix(tmp, dimnames = list(names(tmp), rownames(x)[i])))
		}	
		#	remove empty plots
		tmp <- tmp[rowSums(tmp != 0) > 0, , drop = FALSE]		
		#	remove empty species
		tmp <- tmp[, colSums(tmp != 0) > 0, drop = FALSE]
		#	assign slot species
		res@species <- as.data.frame(tmp, stringsAsFactors = FALSE)
        #	melt to long format
		cov <- array(t(tmp))
		plot <- rep(dimnames(tmp)[[1]], each = dim(tmp)[2])
		abbr.layer <- strsplit(
			rep(dimnames(tmp)[[2]], dim(tmp)[1]), "@", fixed = TRUE)
		abbr <- vapply(abbr.layer,
			FUN = function (x) x[1], FUN.VALUE = character(1))
		layer <- vapply(abbr.layer,
			FUN = function (x) x[2], FUN.VALUE = character(1))
				
		res@species.long <- data.frame(plot, abbr, layer, cov,
			stringsAsFactors = FALSE)
       	res@species.long <- res@species.long[res@species.long$cov != 0, ]
       	#	new layer order
       	layer <- as.character(unique(res@species.long$layer))
       	layer <- layer[match(Layers(x), layer)]
       	layer <- layer[!is.na(layer)]
		#	subset sites
		res@sites <- res@sites[match(rownames(tmp),	rownames(Sites(res))), ]
		if (any(sapply(res@sites, is.na))) {
			stop("NAs introduced in Sites(obj)")
		}	   
		if (length(res@group) != 0) {
		res@group <-
			res@group[names(res@group) %in%
				rownames(tmp)]
		}
		#	method Abbreviation relies on already subsetted taxonomy!
		abbr <- sapply(strsplit(names(res), "@", fixed = TRUE),
		   function (x) x[1])
 		#	subset taxonomy, layers and spatial slots
		res@taxonomy <- res@taxonomy[res@taxonomy$abbr %in% abbr, ]
		res@layers <- layer 
		res@sp.points <- res@sp.points[match(rownames(tmp), res@sp.points$plot), ]
		res@sp.polygons <- res@sp.polygons[match(rownames(tmp), res@sp.points$plot), ]

	    return(res)
    }
)

#	rbind like method to fuse objects
.rbind.VegsoupData <- function (..., deparse.level = 1) {
	allargs <- list(...)
	#	allargs <- list(dta1, dta2)
	#	test scale
	test <- length(unique((sapply(allargs, function (x) AbundanceScale(x)$scale))))
	if (test != 1) {
		stop("\n scale is not the same for all objects")
	}  else {
		scale <- sapply(allargs[1], AbundanceScale, simplify = FALSE)[[1]]
	}
	#	species	
	rows <- vapply(allargs,
		FUN = function (x) nrow(slot(x, "species.long")),
		FUN.VALUE = integer(1))
	
    x <- as.data.frame(matrix("", nrow = sum(rows), ncol = 4),
    	rownames = 1:sum(rows), stringsAsFactors = FALSE)
    names(x) <- c("plot", "abbr", "layer", "cov")
    x$plot <- unlist(sapply(allargs,
    	FUN = function (x) slot(x, "species.long")$plot))
    x$abbr <- unlist(sapply(allargs,
    	FUN = function (x) slot(x, "species.long")$abbr))    
    x$layer <- unlist(sapply(allargs,
    	FUN = function (x) slot(x, "species.long")$layer))        
    x$cov <- unlist(sapply(allargs,
    	FUN = function (x) slot(x, "species.long")$cov))
	
	#x <- x[order(x$plot, x$abbr, x$layer), ]
	#	sites
	y <- do.call("rbind", sapply(allargs, SitesLong, simplify = FALSE))
  	
	#	copied from Vegsoup
	y <- reshape(y[, 1:3],
		direction = "wide",
		timevar = "variable",
		idvar = "plot")
		
	options(warn = -1)
	y <- as.data.frame(
		sapply(y,
		function (x) {
			if (!any(is.na(as.numeric(x)))) {
				x <- as.numeric(x)
			} else {
				x <- as.character(x)	
			}
		}, simplify = FALSE),
		stringsAsFactors = FALSE)
	options(warn = 0)

 	#	groome names
 	names(y) <- gsub("value.", "", names(y), fixed = TRUE)
    #	assign row names
	rownames(y) <- y$plot 
	y <- y[, -grep("plot", names(y))]
	#	set NAs
	y[is.na(y)] <- 0
	#	order to x
	y <- y[match(unique(x$plot), rownames(y)), ]
	#	change longitude column!
	sel <- grep("longitude", names(y))
	y[, sel] <- paste(as.character(y[, sel]), "E", sep = "")
    #	taxonomy
	z <- do.call("rbind", sapply(allargs, Taxonomy, simplify = FALSE))
	z <- unique(z)
	z <- z[order(z$abbr), ]
	#	spatial
	pts <- do.call("rbind",
		sapply(allargs, SpatialPointsVegsoup, simplify = FALSE))
#	cbind(unique(x$plot), pts$plot)
#	check validity of polygon IDs		
	pgs <- sapply(allargs, function (x) as(x@sp.polygons, "SpatialPolygons"), simplify = FALSE)
	pgs <- do.call("rbind", pgs)
	pgs <- SpatialPolygonsDataFrame(pgs, data = pts@data, match.ID = FALSE)	

	res <- new("Vegsoup",
		species.long = x,
		sites = y, 
		taxonomy = z,
		scale = as.list(scale),
		layers = as.character(unique(x$layer)),
		group = rep(integer(1), nrow(y)),
		sp.points = pts,
		sp.polygons = pgs
		)
	res <- VegsoupData(res)	    
}

#if (!isGeneric("rbind")) {
	setGeneric("rbind", function(..., deparse.level = 1)
		standardGeneric("rbind"),
		signature = "...")
#}

setMethod("rbind",
    signature(... = "VegsoupData"),
	.rbind.VegsoupData
)

#	Layers method
"LayersVegsoupData" <- function (obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0, verbose = FALSE) {
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
	if (inherits(obj, "VegsoupData")) {
		res <- as(obj, "Vegsoup")
	} else {
		res <- obj
	}

	species <- SpeciesLong(res)
	scale <- AbundanceScale(res)

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
		
		#	also need to subset spatial objects!
		
#		if (length(unique(SitesLong(res)$plot)) > length(unique(species$plot))) {
#			warning("also some plots will be dropped", call. = FALSE)
#			res@sites.long <- SitesLong(res)[SitesLong(res)$plot %in% species$plot, ]
#		}		
	}	

	species$layer <- factor(species$layer)
	levels(species$layer) <- collapse[match(levels(species$layer), collapse[, 1]), 2]
	species$layer <- as.character(species$layer)

	scale.is.character <- is.character(species$cov)
	
	#	convert original abundance scale to numeric to allow calculations
	if (scale.is.character) {
		species$cov <- as.factor(species$cov)
		levels(species$cov) <- scale$lims[match(levels(species$cov), scale$codes)]
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
		if (scale$scale != "frequency") {
		aggregate(cov ~ plot + abbr + layer, data = species,
				FUN = function (x) {
					round((1 - prod(1 - x / max(scale$lims))) * max(scale$lims), dec)
				})
		} else {
			aggregate(cov ~ plot + abbr + layer, data = species,
				FUN = function (x) {
					round((1 - prod(1 - x / 100)) * 100, dec)	
					})
		}		
	})
	
	species <- species[order(species$plot, species$layer, species$abbr), ]
	
	if (scale$scale != "frequency") {
		if (any(max(species$cov) > max(scale$lims))) {
			warning("\n reduced maximum aggregated abundance value to fit into limits: ",
				min(scale$lims)[1], " to ", max(scale$lims), call. = FALSE)
			species$cov[species$cov >  max(scale$lims)] <- max(scale$lims)
		}
	}
	species$cov <- ceiling(species$cov)
	
	#	back convert to original abundance scale if it was character
	if (scale.is.character) {
		species$cov <- as.character(cut(species$cov, breaks = c(0, scale$lims), labels = scale$codes))
	}
	
	res@species.long <- species
	res@layers <- unique(collapse[, 2])
	res <- VegsoupData(res)
	
	return(invisible(res))

	}
	}
}

setMethod("Layers",
   signature(obj = "VegsoupData"),
    LayersVegsoupData
)

#	Species richness of data set
setGeneric("Richness",
	function (obj, ...)
		standardGeneric("Richness")
)
setMethod("Richness",
    signature(obj = "VegsoupData"),
	function (obj, choice = c("dataset", "sample"), ...) {
		#	obj = sub
		choices <- c("dataset", "sample")
		if (missing(choice)) {
			choice <- "dataset"
		}
		choice <- choices[pmatch(choice, choices)]
		if (is.na(choice)) {
			choice <- "dataset"
		}	
		
		switch(choice, "dataset" = {
			res <- length(unique(DecomposeNames(obj)$abbr))
		}, "sample" = {
		#	slow but reliable	
			res <- rowSums(Layers(obj, aggregate = "layer", verbose = FALSE))
		})		

		return(res)
	}
)

#	connectedness of dissimilarities
#	to do: documenation
setGeneric("getDistconnected",
	function (obj, ...)
		standardGeneric("getDistconnected")
)
setMethod("getDistconnected",
	signature(obj = "VegsoupData"),
	function (obj, dis = "bray", ...) {
		distconnected(vegdist(as.numeric(obj), dis), ...)
	}
)

#	matrix fill
#	used in summary
if (!isGeneric("MatrixFill")) {
setGeneric("MatrixFill",
	function (obj)
	standardGeneric("MatrixFill"))
}

setMethod("MatrixFill",
    signature(obj = "VegsoupData"),
    function (obj) {
		x <- nrow(obj)
		y <- sum(dim(obj))
		res <- x/y * 100
		#	single plot object
		if (x == 1) {
			res <- 100
		}
		return(res)
	}
)

#	summary method
#	to do: documenation
setMethod("summary",
    signature(object = "VegsoupData"),
    function (object, choice = c("all", "species", "sites"), ...) {

	if (missing(choice)) {
		choice <- "all"
	}
	choices <- c("all", "species", "sites")
	choice <- choices[pmatch(choice, choices)]
	if (is.na(choice)) {
		choice <- "all"
	}
	cat("object of class", class(object), "\n")
	species.summary <- paste(
		"\n species (including layer duplictes): ", Richness(object),
		"\n sites (sample plots): ", dim(object)[1],
		"\n matrix fill: ", round(MatrixFill(object), 0), " %",
		"\n layers: ", length(Layers(object)),
		" (", paste(Layers(object), collapse = ", "), ")",
		"\n abundance scale: ", AbundanceScale(object)$scale,
		ifelse(length(object@taxonomy) > 0,
			"\n taxomomy lookup table: supplied ",
			"\n taxomomy lookup table: has non matching taxa!"),
		sep = ""
	)
	if (dim(object)[1] == 1) {
		species.list <- SpeciesLong(object)
		species.list$taxon <-
			Taxonomy(object)$taxon[match(species.list$abbr, Taxonomy(object)$abbr)]
		species.list <- species.list[, c(1,5,3,4)]
		species.list <- apply(species.list[, -1], 1,
			function (x) paste(x[1], " (", x[2], ") ", x[3], sep = "", collpase = ","))
	}		

			#species.list <- paste(species.list, collapse = ", ")
	switch(choice, "all" = {
		cat(species.summary)
		if (dim(object)[1] == 1) cat("\n species list\n", species.list)
		cat("\nsites ")	
		str(Sites(object), no.list = TRUE)
	}, "species" = {
		cat(species.summary)
		if (dim(object)[1] == 1) cat("\nspecies", species.list)
	}, "sites" = {
		cat("\nsites ")
		str(Sites(object), no.list = TRUE)		
	})
	cat("\n    proj4string:\n", proj4string(object))
	cat("\n    bbox:\n"); bbox(object)		
}
)

#	Vegsoup inherits show method
#	to do: check summary method for class VegsoupDataPartitionFidelity
#setMethod("show",
#   signature(object = "VegsoupData"),
#    function (object) {
#			summary(object)
#    }
#)

#	plot method
setMethod("plot",
	signature(x = "VegsoupData", y = "missing"),
	function (x, ...) {
		print(summary(x))
		stop("plot method not implemented yet")	
	}	
)

#	sample data, usally without replacement
#if (!isGeneric("SampleVegsoup")) {
setGeneric("SampleVegsoup", function (x, size, replace = FALSE, prob = NULL)
	standardGeneric("SampleVegsoup"))
#}
#	warning: does not behave as expected for the user
#	StablePartition() relies on this method
#	to do: documentation
#	think about a method for class (VegsoupDataPartition) to sample conditional on Partitiong(obj)
setMethod("SampleVegsoup",
    signature(x = "VegsoupData"),
    function (x, size, replace = FALSE, prob = NULL) {
    	#	for sample the default for size is the number of items inferred from the first argument
		if (missing(size)) {
            size <- dim(x)[1]
        }    
		sel <- sample(1:dim(x)[1], size, replace = replace, prob = prob)
    	if (any(table(sel) > 1)) {
    		sel <- sort(unique(sel))
    		warning("\n replace was set to ", replace,
    			", can only select unique plots! A subsample will be returend!", call. = FALSE)
    	}
    	res <- x[sel, ]
    	return(invisible(res))
    }
)

#	Function to rearrange object (species and sites data frames)
#	by various reordering methods as option.
#	Currently only presence/absencse data is used,
#	and default options of methods apply.
#	Currently there is no way to pass down arguments
#	to functions?
#	

#	arrange und unpartitioned data set
#	to do: documentation
#	rewrite to accept argument binary = FALSE, high priority
#	or use VegsoupDataPartition

setGeneric("Arrange",
	function (object, ...)
		standardGeneric("Arrange")
)
setMethod("Arrange",
    signature(obj = "VegsoupData"),
	function (object, method = c("dca", "hclust", "ward", "flexible", "pam", "packed"), dist = "bray", ...) {
	#	object = dta
	
	if (missing(method)) {
		method  <- "dca"
	} else {
		method <- match.arg(method)			
	}

	si.dis <- vegdist(as.binary(object), method = dist)
	sp.dis <- vegdist(t(as.binary(object)), method = dist)
	
	switch(method, dca = {
		use <- try(decorana(as.binary(object)), silent = TRUE, ...)
		if (inherits(use, "try-error")) {
			use  <- NULL
		}	

		if (is.list(use)) {	
			tmp <- scores(use, choices = 1, display = "sites")
			si.ind <- order(tmp)
			sp.ind <- try(order(scores(use, choices = 1, 
                  display = "species")))
			if (inherits(sp.ind, "try-error")) {
				sp.ind <- order(wascores(tmp, object))
			}
		} else {
			si.ind <- 1:dim(as.binary(object))[1]
			sp.ind <- 1:dim(as.binary(object))[2]
		}
		}, hclust = {
			si.ind <- hclust(si.dis,
				method = "ward")$order
			sp.ind <- hclust(sp.dis,
				method = "ward")$order
		}, ward = {
			si.ind <- agnes(si.dis, diss = TRUE,
				method = "ward")$order
			sp.ind <- agnes(sp.dis, diss = TRUE,
				method = "ward")$order
		}, flexible = {
		   	alpha <- 0.625
	   		beta = 1 - 2 * alpha
		   	si.ind <- agnes(si.dis, method = "flexible",
		   		par.meth = c(alpha, alpha, beta, 0))$order
	   		sp.ind <- agnes(sp.dis, method = "flexible",
	   			par.meth = c(alpha, alpha, beta, 0))$order
		}, pam = {
			si.ind <- pam(si.dis, diss = TRUE)$order
			sp.ind <- pam(sp.dis, diss = TRUE)$order	
		}, packed = {
			si.ind <- order(apply(as.binary(object), 1, sum), decreasing = TRUE)
			sp.ind  <- order(apply(as.binary(object), 2, sum), decreasing = TRUE)
		}
	)
	
	res <- object[si.ind, sp.ind]

	return(res)

	}
)

#if (!isGeneric("DecomposeNames")) {
setGeneric("DecomposeNames",
	function (obj, ...)
		standardGeneric("DecomposeNames")
)
#}

#	getter method Abbrviation DecomposeNames
#	convert abbr to taxon names from species matrix slot(obj, "species")
#	to do: documentation
setMethod("DecomposeNames",
	signature(obj = "VegsoupData"),
	function (obj, verbose = FALSE) {
	#	obj <- dta; type = "nospace"
	if (verbose) {
		cat("\n use Vegsoup standard pattern taxa coding, blanks are dots")
	}

	#	VegsoupData(obj) constructs abbreviated taxa names
	#	pasting make.names and layer seperated with '@' 
	
	#	deparse compound taxon abbreviation and layer string
	#	seperator is '@'

	abbr <- sapply(strsplit(names(obj), "@", fixed = TRUE),
		   function (x) x[1])
	layer <- sapply(strsplit(names(obj), "@", fixed = TRUE),
		   function (x) x[2])

	taxon <- Taxonomy(obj)$taxon[match(abbr, Taxonomy(obj)$abbr)]
	res <- data.frame(abbr.layer = names(obj), abbr, layer, taxon, stringsAsFactors = FALSE)

	if (all(is.na(res$layer))) {
		warning("\n unable to deparse layer string, consider setting type to nospace", call. = FALSE)
	}
	if (all(is.na(res$taxon))) {
		warning("\n unable to deparse taxon string, consider setting type to nospace", call. = FALSE)
	}
	return(invisible(res))
	}
)

#	getter method Abbrviation
#	to do: documentation
setMethod("Abbreviation",
    signature(obj = "VegsoupData"),
    function (obj, ...) DecomposeNames(dta)$abbr
)

#	congruence between indicator and target species.
#	Halme's indicator power
#	to do: documentation
#if (!isGeneric("Indpower")) {			
setGeneric("Indpower",
	function (obj, ...)
		standardGeneric("Indpower")
)
#}
setMethod("Indpower",
    signature(obj = "VegsoupData"),
    function (obj, ...) {
    	res <- indpower(as.binary(obj), ...)
    	diag(res)  <- NA
    	if (type == 0)
			res <- rowMeans(res, na.rm = TRUE)
		return(res)    	
    }
)


#	compositional indicator species analysis
#	to do: documentation
#if (!isGeneric("Indspc")) {	
setGeneric("Indspc",
	function (obj, ...)
		standardGeneric("Indspc")
)
#}
setMethod("Indspc",
    signature(obj = "VegsoupData"),
    function (obj, method, ...) {
    	if (inherits(obj, "VegsoupDataPartition")) {
    		dis <- vegdist(as.binary(obj), obj@dist)
    	} else {
   			if (missing(method)) {    			
				dis <- vegdist(as.binary(obj), "bray")
    		} else {
    			dis <- vegdist(as.binary(obj), ...)
    		}    		
  	 	}
    	res <- indspc(as.binary(obj), dis = dis, ...)
		return(res)    	
    }
)

#	create several clusterings along a vector
#	suitable for plotting
#	return a list
#	to do: rewrite to complement VegsoupDataOptimStride, high priority!
#	and make both function a pair with similar arguments
#	maybe make it a function returning class(VegsoupDataOptimStride)?
#	currently returns a list

.strideVegsoupData <- function (obj, method, stride, fidelity.method, partition.method, mode, verbose = TRUE, alpha = 0.05, ...) {
#	obj = dta	
	if (missing(fidelity.method))
		fidelity.method = "IndVal.g"
	if (missing(partition.method))
		partition.method = "flexible"
	if (missing(stride))
		stride = ceiling(ncol(obj) / 10)
	if (missing(mode))
		mode = 0
	if (verbose) {
		cat("compute", stride, "partitions for stride")
		cat("\nrun SigFidelity with mode", mode)
	}

res <- vector("list", length = stride)
names(res) <- 1:stride

if (verbose) {
	pb.stride <- txtProgressBar(min = 1, max = stride,
	char = '.', width = 45, style = 3)
}

cpu.time <- system.time({
for (i in 1:stride) {
	if (verbose) {
		setTxtProgressBar(pb.stride, i)
	}
	i.prt <- VegsoupDataPartition(obj, k = i, method = partition.method)
	i.fid <- Fidelity(i.prt, method = fidelity.method, verbose = FALSE)
	stat.fid <- apply(getStat(i.fid), 1, sum)#, 2)

	if (i > 1) {
		i.sig.fid <- SigFidelity(i.prt, verbose = FALSE)
		i.sig.fid <- length(which(i.sig.fid$stat < alpha))
	} else {
		i.sig.fid <- 0
	}
	res[[i]] <- list(stat = stat.fid, n.sig = i.sig.fid)
}


stat <- sapply(res, function (x) x[[1]])
n.sig <- sapply(res, function (x) x$n.sig)
diff.stat <- t(rbind(0, apply(stat, 1, diff)))
diff.stat <- apply(diff.stat, 2,
	function(x)	c(sum(x[x > 0]), sum(x[x < 0])))
})	
if (verbose) {
	close(pb.stride)
	cat("\n  computed stride in", cpu.time[3], "sec")
}
res <- list(
	stat = stat,
	n.sig = n.sig,
	diff.stat = diff.stat)
	
return(invisible(res))
}

setGeneric("Stride",
	function (obj, ...)
		standardGeneric("Stride")
)
setMethod("Stride",
	signature(obj = "VegsoupData"),
	.strideVegsoupData	
)

#	plotting method for stride list
plotStride <- function (x) {
#	x = Stride(dta)
	xx <- 1:(ncol(x$stat)) # getK(obj)
	y1 <- x$diff.stat[1, ]
	y2 <- x$diff.stat[2, ]
	ylim  <- range(x$diff.stat)
	
	r1 <- t(rbind(xright = xx - 0.5, ybottom = 0,
		xright = xx + 0.5, ytop = y1))
	r2 <- t(rbind(xright = xx - 0.5, ybottom = y2,
		xright = xx + 0.5, ytop = 0))
	bars <- cbind(xx, c(x$n.sig))[-1,] #?
	#	out.rug <- x@outgroups[,1]

	#	open plot
	par(mar= rep(4,4))
	plot(xx, y1, xlim = c(1, max(xx)),
		ylim = range(x$diff.stat), type = "n",
		xlab = "Cluster", ylab = "Index", frame.plot = FALSE) # getIndex(obj)
	#	sums of positive differences
	apply(r1, 1, function (x) rect(x[1], x[2], x[3], x[4],
		col = "grey50"))
	#	sums of negative differences
	apply(r2, 1, function (x) rect(x[1], x[2], x[3], x[4],
		col = "grey90"))
	#	number of significant indicator species
	plot.window(xlim = range(xx), ylim = range(bars[,2]))
	lines(xx[-1], bars[,2], type = "b",
		pch = 16, col = 2, lwd = 2, cex = 1)
	axis(4, col = 1, lwd = 1)	
	#	sum of all differnences
	plot.window(xlim = range(xx),
		ylim = range(apply(x$diff.stat, 2, sum)))
	lines(xx, apply(x$diff.stat, 2, sum),
		type = "b", pch = 15, cex = 1)
	
	#	mean significant indicator value	
#	plot.window(xlim = range(xx), ylim = range(apply(x$stat, 2, mean)))
#	lines(xx, apply(x$stat, 2, mean), type = "b", pch = 16, cex = 0.5)
		
	legend("top", lty = 1, col = c(1,2),
		legend = c("sum of all differences",
		"number of significant indicator species"),
		bty = "n")

#	if (any(out.rug > 0)) {
#		rug(out.rug[out.rug > 0], side = 3)
#		text(out.rug[out.rug > 0], par("usr")[4] * 1.1,
#			xpd = T, srt = 90)
#	}
}


#	spatial methods
#	to do: rewrite?
#setMethod("coordinates",
#  signature(obj = "VegsoupData"),
#  function (obj) coordinates(obj@sp.points)
#)