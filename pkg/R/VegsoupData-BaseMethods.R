#	internal function to cast species matrix
.cast <- function (obj, mode, ...) {
	#	obj = dta; mode = 1
			
#	cpu.time <- system.time({
    		
	#	slots
	plot <- Species(obj)$plot
	abbr <- Species(obj)$abbr
	layer <- Species(obj)$layer	
	cov <- Species(obj)$cov
	scale <- coverscale(obj) # rename local object scale to ?
	
	#	matrix dimensions
	plots <- unique(plot)
	species.layer <- file.path(abbr, layer, fsep = "@") # faster than paste

	#	resort to layer
	if (length(Layers(obj)) > 1) {	
	#	rather slow, but ensures order
		species <- unique(as.vector(unlist(
			sapply(Layers(obj),
				function (x) {
					species.layer[layer == x]
				}
			))))
	} else {
	#	simple and faster if there is only one layer	
		species <- unique(species.layer)	
	}
			
	#	cover transformation
	if (mode == 1 & !is.null(scale@codes)) {
		cov <- as.numeric(as.character(
			factor(cov, levels = scale@codes, labels = scale@lims)
			))
		if (any(is.na(cov))) {
			stop("cover scale codes do not match data" )
		}	
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
				#	maybe change to "."
				#	there are several function that look for 0!
				r[] <- "0"
				r[match(species.layer[plot == x], species)] <- cov[plot == x]
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

#	internal function to melt sites data.frame
.melt <- function (obj) {
	#	obj = dta
	res <- stack(
			data.frame(plot = rownames(slot(obj, "sites")),
				slot(obj, "sites"), stringsAsFactors = FALSE),
		stringsAsFactors = FALSE) 

	plot <- res[res$ind == "plot", ]$values
	plot <- rep(plot, (nrow(res) / length(plot)) - 1)
	res <- res[!res$ind == "plot", ]
	res <- data.frame(
		plot = as.character(plot),
		variable = as.character(res[, 2]),
		value = as.character(res[, 1]),
		stringsAsFactors = FALSE)
	res <- res[order(res$plot), ]
	res[is.na(res)] <- ""

	rownames(res) <- 1:nrow(res)
	res	
}

#	generating function
#	to do: documentation, high priority!

VegsoupData <- function (obj, decostand, dist, verbose = FALSE) {
	require(stats)
	#	obj <- qry; verbose = TRUE	
	if (!inherits(obj, "Vegsoup")) {
		stop("Need object of class Vegsoup")
	}

	if (missing(decostand)) {
		decostand = new("decostand", method = NULL)
	} else {
		decostand = new("decostand", method = decostand)
	}
	
	if (missing(dist)) {
		dist = "euclidean"
	}
		
	scale <- coverscale(obj)
	lay <- Layers(obj)
	txa <- Taxonomy(obj)
	species <- Species(obj)

	#	rewrite to use .cast()
	#	silenced, if ok delete

	if (FALSE) { 	
	#	 casts species matrix for Braun-Blanquet scales	
	if (scale$scale == "Braun-Blanquet" | scale$scale == "Braun-Blanquet 2") {
		if (!is.character(species$cov)) {
			stop("Abundance scale should be of mode character")
		}
		if (length(lay) == 1) {
			if (verbose) cat("\ndata is structered in only one layer")
		} else {
			if (verbose) cat("\ndata is structered in layers: ", lay)
		}
		
	#	species <- as.data.frame(.cast(obj, mode = 2),
	#		stringsAsFactors = FALSE)
			
	} # end if "Braun Blanquet" | "Braun-Blanquet 2"

	#	cast species matrix for Domin scale
	#	this is Braun-Blanquet scale
	

	if (scale$scale == "Domin") {
		if (!is.character(species$cov)) {
			if (is.integer(species$cov)) {
				warning("\n Codes for Domin scale supplied as integer, ",
					"attempt to change mode to charcter", call. = FALSE)
				species$cov <- as.character(species$cov)
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

		if (!is.numeric(species$cov)) {
			#	warning("changed mode to numeric", str(species$cov))
			mode(species$cov) <- "numeric"
		}	
		xt  <- xtabs(cov ~ plot + abbr + layer,
			data = species)
	
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
	}
	
	#	develop class VegsoupData from class Vegsoup
	res <- new("VegsoupData", obj)

	#	assign class slots
	res@decostand = decostand
	res@dist = dist

	return(res)
}

#	return species matrix
setMethod("as.numeric",
    signature(x = "VegsoupData"),
    function (x, mode) {
    	if (missing(mode)) mode <- "Q"
    	MODE <- c("Q", "R")
    	mode <- match.arg(toupper(mode), MODE)
    	m <- .cast(x, mode = 1)
		#	standardization as definded by decostand(x)		
		stand <- slot(slot(x, "decostand"), "method")
    	
		if (!is.null(stand)) {
			if (length(stand) < 2) {
				if (stand == "wisconsin") {
					stand <- c("max", "total")
					m <- vegan::decostand(m, "max", 2)
					m <- vegan::decostand(m, "total", 1)
				} else {
					m <- vegan::decostand(m, stand)
				}
			} else {
				for (i in stand) {
						m <- vegan::decostand(m, i)	
					}
			}
			attributes(m)$decostand <- stand 
		}
   	if (mode == "R") m <- t(m)
   	return(invisible(m))
	}

)

setMethod("as.character",
    signature(x = "VegsoupData"),
    function (x, mode) {
    	if (missing(mode)) mode <- "Q"
    	MODE <- c("Q", "R")
    	mode <- match.arg(toupper(mode), MODE)
    	m <- .cast(x, mode = 2)
   		if (mode == "R") m <- t(m)
   		return(invisible(m))
    }
)
	
setMethod("as.logical",
    signature(x = "VegsoupData"),
    function (x, mode) {
    	if (missing(mode)) mode <- "Q"
    	MODE <- c("Q", "R")
    	mode <- match.arg(toupper(mode), MODE)
    	m <- .cast(x, mode = 3)
   		if (mode == "R") m <- t(m)
   		storage.mode(m) <- "integer"
   		return(invisible(m))    	
    }
)	

#if (!isGeneric("as.matrix")) {
#	setGeneric("as.logical")
#}
#if (!isGeneric("rowSums")) {
setGeneric("as.matrix",
	function (x, ...)
	standardGeneric("as.matrix"))
#}
setMethod("as.matrix",
    signature(x = "VegsoupData"),
    function (x, typeof, ...) {
    	if (missing(typeof)) typeof <- "numeric"    		
    	TYPEOF <- c("character", "numeric", "logical")
    	typeof <- match.arg(typeof, TYPEOF)

    	if (typeof == "character") {
    		m <- as.character(x, ...)
    	}
    	if (typeof == "numeric") {
    		m <- as.numeric(x, ...)
    	}
    	if (typeof == "logical") {
    		m <- as.logical(x, ...)
    	}
    	return(m)
    }    	    
)
setAs(from = "VegsoupData", to = "matrix",
	def = function (from) {
		as.matrix(from)
		# typeof = "character", mode = "Q"
	}
)
#	ensure that also base functions dispatch properly
as.array.VegsoupData <- as.matrix.VegsoupData <-
	function (x, ...) as.matrix(x, ...) # as(x, "matrix")

setMethod("as.vector",
	signature(x = "VegsoupData", mode = "missing"), # 
	  function (x, mode) {
	  	if (missing(mode)) mode = "numeric"
	  	as.vector(as.matrix(x, typeof = mode))
})
#	ensure that base functions calling as.vector() work
as.vector.VegsoupData <- function (x, mode) {
	if (missing(mode)) mode = "numeric"
	as.vector(as.matrix(x, typeof = mode))
}	

#	locations and values of nonzero entries
#if (!isGeneric("indices")) {

setGeneric("indices",
	function (x, ...)
	standardGeneric("indices"))	
#}	
setMethod("indices",
	signature(x = "VegsoupData"), # 
	  function (x, typeof) {
    	if (missing(typeof)) typeof <- "numeric"    		
    	TYPEOF <- c("character", "numeric", "logical")
    	typeof <- match.arg(typeof, TYPEOF)
    	
		sc <- coverscale(x)
		al <- file.path(Species(x)$abbr, Species(x)$layer, fsep = "@")
		ual <- unique(al)
		pl <- Species(x)$plot
		upl <- unique(pl)
		if (typeof == "numeric" & !is.null(sc@codes)) {
			cv <- as.numeric(as.character(
				factor(Species(x)$cov, levels = sc@codes, labels = sc@lims)
				))
		}
		if (typeof == "character") {
			cv <- Species(x)$cov
		}
		if (typeof == "logical") {
			cv <- rep(1, nrow(Species(x)))
		}
		i <- match(al, ual)
		j <- as.integer(ordered(pl, levels = upl))
		list(i = i, j = j, x = cv, dimnames = list(ual, upl))
		}
)

#	coerce to sparse matrix
#	very basic!
setAs(from = "VegsoupData", to = "sparseMatrix",
	def = function (from) {
		require(Matrix)
		ij <- indices(from)
		res <- sparseMatrix(ij$i, ij$j, x = as.integer(ij$x),
			dimnames = ij$dimnames)
		res
	}
)

setAs(from = "VegsoupData", to = "dsparseMatrix",
	def = function (from) {
		require(Matrix)
		ij <- indices(from)
		res <- sparseMatrix(ij$i, ij$j, x = ij$x,
			dimnames = ij$dimnames)
		res
	}
)

#if (!isGeneric("rownames")) {
setGeneric("rownames", function (x, do.NULL = TRUE, prefix = "row")
	standardGeneric("rownames"))
#}	
setMethod("rownames",
    signature(x = "VegsoupData", do.NULL = "missing", prefix = "missing"),
    function (x) {
    #	warning! order?	
		unique(Species(x)$plot)	
	}
)
#if (!isGeneric("colnames")) {
setGeneric("colnames", function (x, do.NULL = TRUE, prefix = "col")
	standardGeneric("colnames"))
#}
setMethod("colnames",
    signature(x = "VegsoupData"),
    function (x) {
	#	adapted from .cast()
	abbr <- Species(x)$abbr
	layer <- Species(x)$layer
	species.layer <- paste(abbr, layer, sep = "@")
	res <- unique(as.vector(unlist(
			sapply(Layers(x),
				function (x) {
					species.layer[layer == x]
				}
			))))
    res
    }
)
setMethod("dimnames",
    signature(x = "VegsoupData"),
    function (x) {
		list(rownames(x), colnames(x))
	}
)    		

setMethod("names",
    signature(x = "VegsoupData"),
    function (x) {
    	names(Sites(x))
    }
)

#if (!isGeneric("names<-")) {
#setGeneric("names <-",
#	function (x, value, ...)
#		standardGeneric("Sites<-")
#)
#}
#	replacement method for names

#	equivalent	
#setMethod('names<-', signature(x='VegsoupData'), 
#	function(x, value)  {
#		x@sites@names <- value
#		x
#	}
#)
setReplaceMethod("names",
	signature(x = "VegsoupData", value = "character"),
	function (x, value) {
		x@sites@names <- value
		x
	}
)

    	
#if (!isGeneric("nrow")) {
setGeneric("nrow", function(x)
	standardGeneric("nrow"))
#}
setMethod("nrow",
    signature(x = "VegsoupData"),
    function (x) {
		length(rownames(x))
	}
)
#if (!isGeneric("dim")) {
setGeneric("ncol", function (x)
	standardGeneric("ncol"))
#}
setMethod("ncol",
    signature(x = "VegsoupData"),
    function (x) {
		length(colnames(x))
	}
)
#	'dim' is a primitive function
setMethod("dim",
    signature(x = "VegsoupData"),
	    function (x) {
			c(nrow(x), ncol(x))
		}
)
#if (!isGeneric("ncell")) {
setGeneric("ncell",
	function (x, ...)
	standardGeneric("ncell"))
#}
setMethod("ncell",
	signature(x = "VegsoupData"),
	function (x, ...) {
    	prod(dim(x))
    }
)
#if (!isGeneric("rowSums")) {
setGeneric("rowSums",
	function (x, na.rm = FALSE, dims = 1, ...)
	standardGeneric("rowSums"))
#}
setMethod("rowSums",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1, typeof = "logical", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}	
    	rowSums(as.matrix(x, typeof = typeof), ...)
    }
)
#if (!isGeneric("colSums")) {
setGeneric("colSums",
	function (x, na.rm = FALSE, dims = 1)
	standardGeneric("colSums"))
#}
setMethod("colSums",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1, typeof = "logical", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}
    	colSums(as.matrix(x, typeof = typeof), ...)
    }
)
#if (!isGeneric("rowMeans")) {
setGeneric("rowMeans", function (x, na.rm = FALSE, dims = 1, ...)
	standardGeneric("rowMeans"))
#}
setMethod("rowMeans",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1, typeof = "numeric", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}
    	rowMeans(as.matrix(x, typeof = typeof), ...)
    }
)
#if (!isGeneric("rowSums")) {
setGeneric("colMeans", function (x, na.rm = FALSE, dims = 1, ...)
	standardGeneric("colMeans"))
#}
setMethod("colMeans",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1, typeof = "numeric", ...) {
		if (typeof == "character") {
			stop("\n no way to calculate sums based on characters")
		}
    	colMeans(as.matrix(x, typeof = typeof), ...)
    }
)
#	standardisation
#if (!isGeneric("decostand")) {
setGeneric("decostand", function (obj)
	standardGeneric("decostand"))
#}
#if (!isGeneric("decostand<-")) {
setGeneric("decostand<-",
	function (obj, value, ...)
		standardGeneric("decostand<-")
)
#}
setMethod("decostand",
		signature(obj = "VegsoupData"),
	    	function (obj) {
				slot(slot(obj, "decostand"), "method")
			}
)
setReplaceMethod("decostand",
	signature(obj = "VegsoupData", value = "character"),
	function (obj, value) {
		#	taken from vegan
	    METHODS <- c("total", "max", "frequency", "normalize", "range", 
            "standardize", "pa", "chi.square", "hellinger", "log",
            "wisconsin")            
        value <- match.arg(value, METHODS, several.ok = TRUE)		
		value <- new("decostand", method = value)
		obj@decostand <- value		
		return(obj)		
	}
)
setReplaceMethod("decostand",
	signature(obj = "VegsoupData", value = "NULL"),
 	function (obj, value) {
		obj@decostand <- new("decostand", method = NULL)
		return(obj)
	}
)	   
#	dissimilarity
#if (!isGeneric("decostand<-")) {
setGeneric("vegdist",
	function (x, ...)
		standardGeneric("vegdist")
)
#}
#if (!isGeneric("decostand<-")) {
setGeneric("vegdist<-",
	function (x, value, ...)
		standardGeneric("vegdist<-")
)
#}
setMethod("vegdist",
	signature(x = "VegsoupData"),
	function (x, ...) {
		x@dist
	}
)
setReplaceMethod("vegdist",
	signature(x = "VegsoupData", value = "character"),
	function (x, value) {
		#	from vegan::vegdist
		METHODS <- c("manhattan", "euclidean", "canberra", "bray",
		"kulczynski", "gower", "morisita", "horn", "mountford",
		"jaccard", "raup", "binomial", "chao", "altGower", "cao")
		method <- METHODS[pmatch(value, METHODS)]
		x@dist <- method
		x	
	}
)	
#	retrieve distance matrix
#	to do: documentation
setGeneric("as.dist",
	function (m, diag = FALSE, upper = FALSE, ...)
		standardGeneric("as.dist")
)
setMethod("as.dist",
	signature(m = "VegsoupData"),
	function (m, binary, mode, ...) {
		#	as.mumeric and as.logical
		#	automatically apply decostand method!
		#	argument mode controls transpostion before
		#	caluclation of distances
		if (missing(mode)) mode = "Q"
		if (missing(binary)) {
			X <- as.numeric(m, mode = mode)
		} else {
			X <- as.logical(m, mode = mode)	
		}
		Xd <- vegan::vegdist(X, method = m@dist, ...)
		
		#	ensure dissimilarities
		if (max(Xd) > 1) Xd <- Xd / max(Xd)	
		
		#	assign attribute
		attributes(Xd) <- c(attributes(Xd), mode = toupper(mode))
		
		return(Xd)
	}
)

#	connectedness of dissimilarities
#	method for class VegsoupDataPartition, check inheritance should be absolete!
#	to do: documentation
setGeneric("getDistconnected",
	function (obj, ...)
		standardGeneric("getDistconnected")
)

setMethod("getDistconnected",
	signature(obj = "VegsoupData"),
	function (obj, ...) {
		distconnected(as.dist(obj), ...)
	}
)

#	 methods based on functions in 'utils'
#if (!isGeneric("head")) {
setGeneric("head", function(x)
	standardGeneric("head"))
#}

setMethod("head",
    signature(x = "VegsoupData"),
    function (x, choice, typeof, n = 6L, ...) {
	    if (missing(choice))
	    	choice = "species"
    	CHOICE <- c("species", "sites")
    	choice <- CHOICE[pmatch(choice, CHOICE)]
	    if (missing(typeof))
		    typeof = "logical"
	    if (missing(n))
		    n = 6L
    	if (choice == "species")
			res <- head(as.matrix(x, typeof), n, ...)
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
    function (x, n = 6L, choice, typeof, ...) {
	    if (missing(choice))
	    	choice = "species"
		if (missing(typeof))
		    typeof = "logical"
	    if (missing(n))
			n = 6L
		if (choice == "species")
			res <- tail(as.matrix(x, typeof), n, ...)
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
		species = as.matrix(from, typeof = "character", mode = "Q"),
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
		if (!("sites" %in% slotNames(x))) {
			stop("no $ method for object without slot sites")
		}
		return(x@sites[[name]])
		#do.call("$", list = (Sites(x), name))
	}
)

setReplaceMethod("$",
	signature(x = "VegsoupData"),
	function (x, name, value) {
		if (!("sites" %in% slotNames(x)))
			stop("no $<- method for object without attributes")		
 			x@sites[[name]] <- value 	
		return(x)		
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
    function (x, i, j, ..., drop = TRUE) {
	    #	debug
	    #	x = dta; i = 1; j <- c(4,7,9,1,12); j <- rep(TRUE, ncol(x))
		#	x <- prt; i = Partitioning(x) == 2
	    res <- x
	    if (missing(i)) i <- rep(TRUE, nrow(res))
	    if (missing(j)) j <- rep(TRUE, ncol(res))
	    #	change to as.logical(x)[i, j, ...]
		#	when slot species is dropped
		tmp <- as.character(x)[i, j, drop = FALSE]
		#	validity
		if (all(unlist(tmp) == 0)) {
			stop(call. = FALSE, "subset does not contain any species!")
		}
		#	if single plot "[" method will return class character
		if (class(tmp) == "character") {
			tmp <- t(matrix(tmp, dimnames = list(colnames(tmp), rownames(x)[i])))
		}	
		#	remove empty plots
		tmp <- tmp[rowSums(tmp != 0) > 0, , drop = FALSE]		
		#	remove empty species
		tmp <- tmp[, colSums(tmp != 0) > 0, drop = FALSE]
		#	assign slot species
		#	res@species <- as.data.frame(tmp, stringsAsFactors = FALSE)
        #	melt to long format
		cov <- array(t(tmp))
		plot <- rep(dimnames(tmp)[[1]], each = dim(tmp)[2])
		abbr.layer <- strsplit(
			rep(dimnames(tmp)[[2]], dim(tmp)[1]), "@", fixed = TRUE)
		abbr <- unlist(lapply(abbr.layer, "[[", 1))
		layer <- unlist(lapply(abbr.layer, "[[", 2))
		#	class 'species'				
		res@species <- data.frame(plot, abbr, layer, cov,
			stringsAsFactors = FALSE)
       	res@species <- res@species[res@species$cov != 0, ]
       	#	new layer order
       	layer <- as.character(unique(res@species$layer))
       	layer <- layer[match(Layers(x), layer)]
       	layer <- layer[!is.na(layer)]
		#	subset sites
		res@sites <- res@sites[match(rownames(tmp),	rownames(Sites(res))), ]
		if (any(sapply(res@sites, is.na))) {
			stop("NAs introduced in Sites(obj)")
		}	   
		if (length(res@group) != 0) {
			res@group <- res@group[names(res@group) %in% rownames(tmp)]
		}
		#	method Abbreviation relies on already subsetted taxonomy!
		abbr <- unlist(lapply(strsplit(colnames(res), "@", fixed = TRUE), "[[", 1))
 		#	finaly subset taxonomy, layers and spatial slots
		res@taxonomy <- res@taxonomy[res@taxonomy$abbr %in% abbr, ]
		res@layers <- layer 
		res@sp.points <- res@sp.points[match(rownames(tmp),
			SpatialPolygonsVegsoup(res)$plot), ]
		res@sp.polygons <- res@sp.polygons[match(rownames(tmp),
			SpatialPolygonsVegsoup(res)$plot), ]

	    return(res)
    }
)

setReplaceMethod("[", c("VegsoupData", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if (!("sites" %in% slotNames(x)))
			stop("no [[ method for object without slot sites")
		x@sites[[i]] <- value
		x
	}
)

###	warning layers must be equal!!!
#	rbind like method to fuse objects
".rbind.VegsoupData" <- function (..., deparse.level = 1) {
	allargs <- list(...)
	#	allargs <- list(s1, s2, s3)
	#	test if all objects have the same abundance scale
	test <- length(unique((sapply(allargs,
			function (x) coverscale(x)@name))))
	if (test != 1) {
		stop("\n cover scale is not the same for all objects")
	}  else {
		#	fails!
		scale <- sapply(allargs[1], coverscale, simplify = FALSE)
	}
	#	test for overlapping plot ids
	test <- c(sapply(allargs, rownames))
	test <- length(test) == length(unique(test))
	if (!test) {
		stop("\n there seem to be overlapping plot names")
	}
	#	species 'x'	
	j <- vapply(allargs,
		FUN = function (x) nrow(Species(x)),
		FUN.VALUE = integer(1))
    x <- as.data.frame(matrix("", nrow = sum(j), ncol = 4),
    	rownames = 1:sum(j), stringsAsFactors = FALSE)
    names(x) <- c("plot", "abbr", "layer", "cov")
    x$plot <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$plot))
    x$abbr <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$abbr))    
    x$layer <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$layer))        
    x$cov <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$cov))
	#	sites 'y'
	y <- do.call("rbind", sapply(allargs, .melt, simplify = FALSE))
	#	copied from Vegsoup()
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
	#	order y to x
	y <- y[match(unique(x$plot), rownames(y)), ]
	#	change longitude column!
	sel <- grep("longitude", names(y))
	y[, sel] <- paste(as.character(y[, sel]), "E", sep = "")
    #	taxonomy
	z <- do.call("rbind", sapply(allargs, Taxonomy, simplify = FALSE))
	z <- unique(z)
	z <- z[order(z$abbr), ]
	#	spatial points
	pts <- do.call("rbind",
		sapply(allargs, SpatialPointsVegsoup, simplify = FALSE))
	#	order pts to x
	pts <- pts[match(unique(x$plot), pts$plot), ] 
	#	stopifnot(all.equal(unique(x$plot), pts$plot))
	#	spatial polygons
	pgs <- do.call("rbind",
		sapply(allargs, SpatialPolygonsVegsoup, simplify = FALSE))	
	#	polygon IDs
	#ids <- unlist(sapply(allargs, function (x) { 
	#	sapply(slot(SpatialPolygonsVegsoup(x), "polygons"),
	#		function (x) slot(x, "ID")) # "Polygons"
	#}, simplify = FALSE))		
	pgs <- SpatialPolygonsDataFrame(pgs, data = pts@data, match.ID = FALSE)	
	#	order pgs to x
	pgs <- pgs[match(unique(x$plot), pgs$plot), ]
	res <- new("Vegsoup",
		species = x,
		sites = y, 
		taxonomy = z,
		coverscale = scale,
		layers = as.character(unique(x$layer)),
		group = rep(integer(1), nrow(y)),
		sp.points = pts,
		sp.polygons = pgs
		)
	res <- VegsoupData(res)	    
}

#if (!isGeneric("rbind")) {
setGeneric("rbind",
		function (..., deparse.level = 1)
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
setGeneric("richness",
	function (obj, ...)
		standardGeneric("richness")
)
setMethod("richness",
    signature(obj = "VegsoupData"),
	function (obj, choice = c("dataset", "sample"), ...) {
		#	obj = sub
		CHOICE <- c("dataset", "sample")
		if (missing(choice)) choice <- "dataset"
		choice <- CHOICE[pmatch(choice, CHOICE)]
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
		#x <- nrow(obj)
		#y <- sum(dim(obj))
		#res <- x/y * 100
		#	zeros <- 
		res <- sum(as.logical(obj) == 0) / prod(dim(obj))
		res <- (1 - res) * 100
		#	single plot object
		if (nrow(obj) == 1) {
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
		"\n species (discarding layer duplicates): ", richness(object),
		"\n sites (sample plots): ", dim(object)[1],
		"\n matrix fill: ", round(MatrixFill(object), 0), " %",
		"\n layers: ", length(Layers(object)), 
		" (", paste(Layers(object), collapse = ", "), ")",
		"\n abundance scale: ", coverscale(object)@name,
		ifelse(is.null(decostand(object)),
			paste("\n decostand method: undefined (NULL)"),
			paste("\n decostand method: ", decostand(object))
		),		
		"\n dissimilarity: ", object@dist,	   				
		ifelse(length(object@taxonomy) > 0,
			"\n taxomomic reference: valid ",
			"\n taxomomic reference: has non matching taxa!"),
		sep = ""
	)
	if (dim(object)[1] == 1) {
		species.list <- Species(object)
		species.list$taxon <-
			Taxonomy(object)$taxon[match(species.list$abbr, Taxonomy(object)$abbr)]
		species.list <- species.list[, c(1,5,3,4)]
		species.list <- apply(species.list[, -1], 1,
			function (x) paste(x[1], " (", x[2], ") ", x[3], sep = "", collpase = ","))
	}			
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
#	think about a method for class (VegsoupDataPartition) to sample conditional on Partitioning(obj)
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

#	arrange an unpartitioned data set
setGeneric("seriation",
	function (obj, ...)
		standardGeneric("seriation")
)
setMethod("seriation",
    signature(obj = "VegsoupData"),
	function (obj, method, ...) {
	
	if (missing(method)) {
		method  <- "dca"
	} else {
		METHODS <- c("dca", "hclust", "ward", "flexible", "packed")
		method <- match.arg(method, METHODS)
	}
	
	si.dis <- as.dist(obj, "logical")
	sp.dis <- as.dist(obj, "logical", mode = "R")
	
	switch(method, dca = {
		use <- try(decorana(as.matrix(obj)), silent = TRUE, ...)
		if (inherits(use, "try-error")) {
			use <- NULL
		}	
		if (is.list(use)) {	
			tmp <- scores(use, choices = 1, display = "sites")
			si.ind <- order(tmp)
			sp.ind <- try(order(scores(use, choices = 1, 
                  display = "species")))
			if (inherits(sp.ind, "try-error")) {
				sp.ind <- order(wascores(tmp, obj))
			}
		}
		else {
			si.ind <- 1:dim(obj)[1]
			sp.ind <- 1:dim(obj)[2]
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
		}, packed = {
			si.ind <- order(rowSums(dta, "logical"), decreasing = TRUE)
			sp.ind  <- order(colSums(dta, "logical"), decreasing = TRUE)
		}
	)

	res <- obj[si.ind, sp.ind]
	return(res)

	}
)

#if (!isGeneric("DecomposeNames")) {
setGeneric("DecomposeNames",
	function (obj, ...)
		standardGeneric("DecomposeNames")
)
#}

#	getter method Abbreviation DecomposeNames
#	convert abbr to taxon names from species matrix slot(obj, "species")
#	to do: documentation
setMethod("DecomposeNames",
	signature(obj = "VegsoupData"),
	function (obj, verbose = FALSE) {
	#	obj <- dta; type = "nospace"
	if (verbose) {
		cat("\n Vegsoup standard pattern taxa coding:")
		cat("\n blanks are dots, '@' speperates abbreviations and layer")
	}
	abbr.layer <- colnames(obj)	
	abbr <- unlist(lapply(strsplit(abbr.layer, "@", fixed = TRUE), "[[" , 1))
	layer <- unlist(lapply(strsplit(abbr.layer, "@", fixed = TRUE), "[[" , 2))
	taxon <- Taxonomy(obj)$taxon[match(abbr, Taxonomy(obj)$abbr)]
	
	res <- data.frame(abbr, layer, taxon, stringsAsFactors = FALSE)
	rownames(res) <- abbr.layer

	if (any(is.na(res$layer)) | any(is.na(res$taxon))) {
		stop("\n unable to deparse layer string,",
			" consider setting type to nospace", call. = FALSE)
	}
	return(invisible(res))
	}
)

#	getter method Abbrviation
#	to do: documentation
setMethod("Abbreviation",
    signature(obj = "VegsoupData"),
    function (obj, ...) DecomposeNames(obj)$abbr
)

#if (!isGeneric("DecomposeNames")) {
setGeneric("abbr.layer",
	function (obj, ...)
		standardGeneric("abbr.layer")
)
#}

setMethod("abbr.layer",
    signature(obj = "VegsoupData"),
    function (obj, ...) {
    	file.path(obj$abbr, obj$layer, fsep = "@")
    }
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
    	res <- indpower(as.logical(obj), ...)
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
    		dis <- vegdist(as.logical(obj), obj@dist)
    	} else {
   			if (missing(method)) {    			
				dis <- vegdist(as.logical(obj), "bray")
    		} else {
    			dis <- vegdist(as.logical(obj), ...)
    		}    		
  	 	}
    	res <- indspc(as.logical(obj), dis = dis, ...)
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

#SRTM <- function (x) {
#	if (!inherits(obj, "VegsoupData")) stop("Need object inheriting from class VegsoupData")
#	require(geonames)
#	res <- unlist(apply(coordinates(obj), 1, function (x) GNsrtm3(lat = x[2], lng = x[1])[1]))
#}
