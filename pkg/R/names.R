
#if (!isGeneric("rownames")) {
setGeneric("rownames", function (x, do.NULL = TRUE, prefix = "row")
	standardGeneric("rownames"))
#}	
setMethod("rownames",
    signature(x = "Vegsoup", do.NULL = "missing", prefix = "missing"),
    function (x) {
    #	warning! order?	
		unique(Species(x)$plot)	
	}
)

setReplaceMethod("rownames",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {
		#	x <- strauch1992
		#	value = paste("ms92.", rownames(x), sep = "")
		if (inherits(x, "VegsoupPartition")) {
			stop("rownames method fpr VegsoupPartition not yet implemented!")
		}
		
		if (length(value) != nrow(x)) {
			stop("length of values must match nrow(x)")
		}
		xy <- list(
			x = rownames(Sites(x)),
			y = value)
		
		#	species	
		pl <- factor(Species(x)$plot, ordered = FALSE)
		sel <- match(levels(pl), xy$x)
		xy$x <- xy$x[sel]
		xy$y <- xy$y[sel]
		levels(pl) <- xy$y
		
		x@species$plot <- as.character(pl)
		
		#	sites
		sel <- match(rownames(Sites(x)), xy$x)
		rownames(x@sites) <- xy$y[sel]

		#	points
		sel <- match(x@sp.points$plot, xy$x)
		x@sp.points$plot <- xy$y[sel]

		#	polygons
		sel <- match(x@sp.polygons$plot, xy$x)
		x@sp.polygons$plot <- xy$y[sel]
		row.names(x@sp.polygons) <- xy$y[sel]
		#sapply(slot(x@sp.polygons, "polygons"), function (x) slot(x, "ID"))
		
		return(x)
	}
)

#if (!isGeneric("colnames")) {
setGeneric("colnames", function (x, do.NULL = TRUE, prefix = "col")
	standardGeneric("colnames"))
#}
setMethod("colnames",
    signature(x = "Vegsoup"),
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
    signature(x = "Vegsoup"),
    function (x) {
		list(rownames(x), colnames(x))
	}
)
setMethod("names",
    signature(x = "Vegsoup"),
    function (x) {
    	names(Sites(x))
    }
)

setReplaceMethod("names",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {
		x@sites@names <- value
		x
	}
)

#	convert abbr to taxon names
#if (!isGeneric("split.abbr")) {
setGeneric("split.abbr",
	function (obj)
		standardGeneric("split.abbr")
)
#}
setMethod("split.abbr",
	signature(obj = "Vegsoup"),
	function (obj) {
	#	obj <- dta; type = "nospace"

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

#if (!isGeneric("abbr.layer")) {
setGeneric("abbr.layer",
	function (obj, unique)
		standardGeneric("abbr.layer")
)
#}
setMethod("abbr.layer",
    signature(obj = "Vegsoup"),
    function (obj, unique) {
    	if (missing(unique)) {
    		unique = FALSE	
    	}
    	al <- file.path(Species(obj)$abbr, Species(obj)$layer, fsep = "@")
    	if (!unique) {    	
    		return(al)
    	} else {
    		if (length(Layers(obj)) > 1) {
    			#	resort to Layers(obj), copied from .cast()
    			#	speed issue here? see KML section resort to Layers(obj)
    			l <- Species(obj)$layer
				al <- unique(unlist(sapply(Layers(obj), function (x) al[l == x])))
			} else {
				al <- unique(al)	
			}
    		return(al)
    	}
    }
)

#if (!isGeneric("abbr")) {
#	get or set taxon abbreviation
setGeneric("abbr",
	function (obj) {
		standardGeneric("abbr")
	}	
)
#}
### which one below?
setMethod("abbr",
    signature(obj = "Vegsoup"),
    function (obj) {
    	split.abbr(obj)$abbr
    }	
)
setMethod("abbr",
    signature(obj = "Vegsoup"),
    function (obj) {
    	sort(unique(Species(obj)$abbr))
    }	
)

#if (!isGeneric("abbr")) {
#	get or set taxon abbreviation
setGeneric("taxon",
	function (obj) {
		standardGeneric("taxon")
	}	
)
setMethod("taxon",
    signature(obj = "Vegsoup"),
    function (obj) {
    	Taxonomy(obj)$taxon
    }	
)

#setGeneric("abbr<-",
#	function (obj, value, ...)
#		standardGeneric("abbr<-")
#)
#setReplaceMethod("abbr",
#	signature(obj = "Vegsoup", value = "character"),
#	function (obj, value) {
#		#	to do: needs security for all slots!
#		obj@species$abbr <- value		
#		return(obj)		
#	}
#)