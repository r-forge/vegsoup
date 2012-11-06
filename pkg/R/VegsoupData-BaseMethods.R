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
		
		cpu.time <- system.time({
		#	change coverscale to numeric using scale$lims
		#	xtabs does not support non-integer values
		cov.factor <- as.factor(species.long$cov)
		cov.levels <- levels(cov.factor)
		species.long$cov <- as.numeric(cov.factor)
		stopifnot(any(is.na(species.long)) == FALSE)
		
		xt  <- xtabs(cov ~ plot + abbr + layer, data = species.long)
		#	initialise species matrix	
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
		
		#	fill species matrix
		for (i in 1:dim(xt)[3]) {
			sel <- grep(paste("", dimnames(xt)$layer[i], sep = "@"),
				dimnames(res)$abbr, fixed = TRUE)
			res[, sel] <- xt[ , , i]
		}
		
		#	remove layers were species are absent
		res <- res[, colSums(res) > 0]
		mode(res) <- "character"

		#	restore abundance scale
		res[] <- as.character(factor(as.vector(res), labels = c("0", cov.levels)))
		species <- as.data.frame(res, stringsAsFactors = FALSE)
		}) # end system.time
		
	} # end if "Braun Blanquet" | "Braun-Blanquet 2"

	#	cast species matrix for Domin scale
	#	this is Braun-Blanquet scale 
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
		
		cpu.time <- system.time({
		#	xtabs does not support non-integer values!			
		#	workaround: change coverscale to numeric using scale$lims
		cov.factor <- as.factor(species.long$cov)
		cov.levels <- levels(cov.factor)
		species.long$cov <- as.numeric(cov.factor)
		stopifnot(any(is.na(species.long)) == FALSE)
		
		xt  <- xtabs(cov ~ plot + abbr + layer, data = species.long)
		#	initialise species matrix	
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
		
		#	fill species matrix
		for (i in 1:dim(xt)[3]) {
			sel <- grep(paste("", dimnames(xt)$layer[i], sep = "@"),
				dimnames(res)$abbr, fixed = TRUE)
			res[, sel] <- xt[ , , i]
		}
		
		#	remove layers were species are absent
		res <- res[, colSums(res) > 0]
		mode(res) <- "character"
		#	workaround
		#	restore abundance scale to character using scale$codes
		for (i in cov.levels) {
			res[res == i] <- scale$codes[scale$codes == i]
		}
		species <- as.data.frame(res, stringsAsFactors = FALSE)
		}) # end system.time
	} # end if "Domin"
	
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
	
	#	cast sites data
	
	#	check missing values
	if (any(SitesLong(obj)[, 3] == "") | any(is.na(SitesLong(obj)[, 3]))) {
		obj@sites.long[obj@sites.long[, 3] == "", 3] <- 0
		obj@sites.long[is.na(obj@sites.long[, 3]), 3] <- 0
		warning("\n NAs and empty fields (\"\") in supplied sites data",
			" filled with zeros", call. = FALSE)
	}
	sites <- reshape(SitesLong(obj)[, 1:3],
		direction = "wide",
		timevar = "variable",
		idvar = "plot")
	
	#	tune data frame structure
	
	#	check NA's resulting from reshape
	#	missing variables values
	if (any(is.na(sites))) {
		sites[is.na(sites)] <- 0
		warning("\n NAs in casted sites data frame",
			" filled with zeros", call. = FALSE)
		#	paste back to @sites.long
		tmp <- stack(sites)
		tmp[, 1] <- as.character(tmp[, 1])
		tmp[, 2] <- as.character(tmp[, 2])
		plot <- tmp[tmp$ind == "plot",]$values
		plot <- rep(plot, (nrow(tmp)/length(plot)) - 1)
		tmp <- tmp[!tmp$ind == "plot",]
		tmp <- data.frame(plot,
			variable = tmp[, 2],
			value = tmp[, 1])
		tmp <- tmp[order(tmp$plot),]
		tmp$variable <- gsub("value.", "", tmp$variable, fixed = TRUE)
		tmp <- data.frame(as.matrix(tmp), stringsAsFactors = FALSE)
		tmp[is.na(tmp)] <- ""
		rownames(tmp) <- 1:nrow(tmp)
		#	to do testing aginst original @sites.long
		obj@sites.long <- tmp[order(tmp$plot, tmp$variable),]
	}
	
	#	change column mode to numeric if possible
	#	supress warning messages caused by as.numeric(x)
	options(warn = -1)
	sites <- as.data.frame(
		sapply(sites,
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
	names(sites) <- gsub("value.", "",
		names(sites), fixed = TRUE)
	rownames(sites) <- sites$plot
	sites <- sites[, -grep("plot", names(sites))]
	sites <- sites[match(rownames(species), rownames(sites)), ]

	#	develop class VegsoupData from class Vegsoup
	res <- new("VegsoupData", obj)
	#	assign class slot
	res@species = species
	res@sites = sites			

	return(res)
}

###	inherited methods based on 'primitive functions'
#	'names' is a primitive function
setMethod("names",
    signature(x = "VegsoupData"),
    function (x) names(x@species)
)
#	'dim' is a primitive function
#	to do: documentation
setMethod("dim",
    signature(x = "VegsoupData"),
	    function (x) dim(x@species)
)

#	retrieve character matrix
#	to do: documentation, validity chekcs against obj@scale, priority high!	
setMethod("as.character",
    signature(x = "VegsoupData"),
    function (x) {
    	res <- as.matrix(x@species)
    	if (mode(res) != "character") {
	    	mode(res) <- "character"
    		res <- as.data.frame(res, stringsAsFactors = FALSE)
    	} else {
    		res <- x@species
    	}
    	return(invisible(res))
    }
)

###	inherited methods based on functions in 'base'
#if (!isGeneric("rownames")) {
setGeneric("rownames", function (x, do.NULL = TRUE, prefix = "row")
	standardGeneric("rownames"))
#}	
setMethod("rownames",
    signature(x = "VegsoupData", do.NULL = "missing",
    prefix = "missing"),
    function (x) rownames(x@species)
)
#if (!isGeneric("dim")) {
setGeneric("nrow", function(x)
	standardGeneric("nrow"))
#}
#	to do: documentation
setMethod("nrow",
    signature(x = "VegsoupData"),
    function (x) nrow(x@species)
)
#if (!isGeneric("dim")) {
setGeneric("ncol", function(x)
	standardGeneric("ncol"))
#}
#	to do: documentation
setMethod("ncol",
    signature(x = "VegsoupData"),
    function (x) ncol(x@species)
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

#	retrieve numeric matrix as defined in AbundanceScale(obj)
#	print mode uses invisible()?
#	use head(as.numeric(obj))?
#	to do: documentation
#if (!isGeneric("as.numeric")) {
#setGeneric("as.numeric", function (x, ...)
#	standardGeneric("as.numeric"))
#}
setMethod("as.numeric",
    signature(x = "VegsoupData"),
    function (x, verbose = FALSE) {
    	#	x = dta
		if (AbundanceScale(x)$scale == "frequency" | AbundanceScale(x)$scale == "binary") {
			res <- as.matrix(x@species)
			mode(res) <- "numeric"
			res <- as.data.frame(res)
			if (verbose) {
				cat("  species matrix is numeric, scale:",
					AbundanceScale(x)$scale)
			}
		} else {
			res <- x@species
			scale <- AbundanceScale(x)		
			tmp <- as.factor(c(as.matrix(res)))
			levels(tmp) <- scale$lims[match(levels(tmp), scale$codes)]
			tmp <- as.numeric(as.character(tmp))
			tmp[is.na(tmp)] <- 0
			res[, ] <- tmp
		}
		return(invisible(res))   	
    }
)
#	retrieve binary matrix
#if(!isGeneric("as.binary"))
setGeneric("as.binary",
	function (obj, ...)
		standardGeneric("as.binary")
)
#}
#	print mode uses invisible()
#	use head(as.binray(obj))?
#	to do: documentation, possible rename to as.logical? priority high!
#	would than become a already defined generic function
#	be careful when renaming
setMethod("as.binary",
    signature(obj = "VegsoupData"),
    function (obj) {
			res <- obj@species != "0"
		mode(res) <- "numeric"
		res <- as.data.frame(res)
		return(invisible(res))
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
    function (x, n = 6L, choice, ...) {
	    if (missing(choice))
	    	choice <- "species"	
    	if (choice == "species")
    		res <- head(x@species)
    	if (choice == "sites")
    		res <- head(x@sites)
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
    function (x, choice, ...) {
	    if (missing(choice))
	    	choice <- "species"
    	if (choice == "species")
    		res <- tail(x@species, ...)
    	if (choice == "sites")
    		res <- tail(x@sites, ...)
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
		list(species = from@species,
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
	}
)

#	Layers method
#	to do: documenation, implement option drop to drop a specific layer! high priority!

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
		
		if (length(unique(SitesLong(res)$plot)) > length(unique(species$plot))) {
			warning("also some plots will be dropped", call. = FALSE)
			res@sites.long <- SitesLong(res)[SitesLong(res)$plot %in% species$plot, ]
		}		
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
			res <- length(unique(DecomposeNames(obj))$abbr)
		}, "sample" = {
			res <- as.binary(Layers(obj, aggregate = "layer", verbose = FALSE))
			res <- rowSums(res)
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
	return(res)	
})

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
		"\n species: ", Richness(object),
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
		species.list$taxon <- Taxonomy(obj)$taxon[match(species.list$abbr, Taxonomy(obj)$abbr)]
		species.list <- species.list[, c(1,5,3,4)]
		species.list <- apply(species.list[, -1], 1,
			function (x) paste(x[1], " (", x[2], ") ", x[3], sep = ""))
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

#	subsetting method
#	to do: documenation
#	VegsoupDataPartition implemts its own method,
#	but internally coreces to class(VegsoupData)
#	and apllies this method!
setMethod("[",
    signature(x = "VegsoupData",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = TRUE)
    {
	    #	debug
	    #	x = dta; i = 1:3; j <- 1:2; j <- rep(TRUE, ncol(x))
	    res <- x
	    if (missing(i)) i <- rep(TRUE, nrow(res))
	    if (missing(j)) j <- rep(TRUE, ncol(res))
	    
		res@species <- as.character(x)[i, j, ...]
		
		if (all(unlist(res@species) == 0)) stop(call. = FALSE, "subset does not contain any species!")		
		
		#	remove empty plots
		res@species <- res@species[rowSums(res@species != 0) > 0, , drop = FALSE]		
		#	remove empty species
		res@species <- res@species[, colSums(res@species != 0) > 0, drop = FALSE]
		
		#	subset long format
		res@species.long <-
			res@species.long[res@species.long$plot %in%	rownames(res), ]
		res@species.long <-
			res@species.long[paste(res@species.long$abbr,
				res@species.long$layer, sep = "@") %in%	names(res), ]
				
		#	subset sites		
		res@sites <-
			res@sites[match(rownames(res),
				rownames(res@sites)), ]
		if (any(sapply(res@sites, is.na))) stop("Error")
		
		#	prone to error if ordering really matters?
		res@sites.long <-
			res@sites.long[res@sites.long$plot %in%	rownames(res), ]
		res@sites.long <- res@sites.long[order(res@sites.long$plot, res@sites.long$variable), ]
		if (length(res@group) != 0) {
		res@group <-
			res@group[names(res@group) %in%
				rownames(res)]
		}
		#	method Abbreviation relies on already subsetted taxonomy!
		abbr <- sapply(strsplit(names(res), "@", fixed = TRUE),
		   function (x) x[1])
 		#	taxonomy is subsetted!
		res@taxonomy <- res@taxonomy[res@taxonomy$abbr %in% abbr, ]
		res@sp.points <- res@sp.points[res@sp.points$plot %in% rownames(res), ]
		res@sp.polygons <- res@sp.polygons[res@sp.points$plot %in% rownames(res), ]
		res@layers <- as.character(unique(res@species.long$layer))
	    return(res)
    }
)

#	#	coerc

#	sample data, usally without replacement
if (!isGeneric("SampleVegsoup")) {
setGeneric("SampleVegsoup", function (x, size, replace = FALSE, prob = NULL)
	standardGeneric("SampleVegsoup"))
}
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
#sample(dta)
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
			si.ind <- agnes(si.dis, diss = TRUE,
				method = "ward")$order
			sp.ind <- agnes(sp.dis, diss = TRUE,
				method = "ward")$order	
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