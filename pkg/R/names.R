#	for species matrix
if (!isGeneric("rownames")) {
setGeneric("rownames", function (x, do.NULL = TRUE, prefix = "row")
	standardGeneric("rownames"))
}
	
setMethod("rownames",
    signature(x = "Vegsoup", do.NULL = "missing", prefix = "missing"),
    function (x) {
		unique(Species(x)$plot)	
	}
)

.replace.rownames <- function (x, value) {	
	if (length(value) != nrow(x)) {
		stop("length of values must match nrow(x)", call. = FALSE)
	}
	xy <- list(x = rownames(Sites(x)), y = value)
	
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
	x@sp.points$plot <- as.character(xy$y[sel])

	#	polygons
	sel <- match(x@sp.polygons$plot, xy$x)
	x@sp.polygons$plot <- xy$y[sel]
	row.names(x@sp.polygons) <- as.character(xy$y[sel])
	
	return(x)
}
	
setReplaceMethod("rownames",
	signature(x = "Vegsoup", value = "character"),
	.replace.rownames
)	

setReplaceMethod("rownames",
	signature(x = "Vegsoup", value = "integer"),
	.replace.rownames
)

if (!isGeneric("colnames")) {
setGeneric("colnames", function (x, do.NULL = TRUE, prefix = "col")
	standardGeneric("colnames"))
}

setMethod("colnames",
    signature(x = "Vegsoup"),
    function (x) {
		a <- Species(x)$abbr
		l <- Species(x)$layer
		al <- file.path(a, l, fsep = "@") # faster than paste
		res <- unique(unlist(sapply(Layers(x), function (x) al[l == x])))
		if (!is.vector(res)) res <- as.vector(res) # to coerce if there is only one layer
	    res
    }
)

setMethod("dimnames",
    signature(x = "Vegsoup"),
    function (x) {
		list(rownames(x), colnames(x))
	}
)

#	for sites data frame
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

if (!isGeneric("row.names")) {
setGeneric("row.names", function (x)
	standardGeneric("row.names"))
}	

setMethod("row.names",
    signature(x = "Vegsoup"),
    function (x) {
		row.names(Sites(x))	
	}
)

setReplaceMethod("row.names",
	signature(x = "Vegsoup", value = "character"),
	.replace.rownames
)

setReplaceMethod("row.names",
	signature(x = "Vegsoup", value = "integer"),
	.replace.rownames
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