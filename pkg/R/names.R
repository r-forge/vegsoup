#	for species matrix
if (!isGeneric("rownames")) {
setGeneric("rownames", function (x, do.NULL = TRUE, prefix = "row")
	standardGeneric("rownames"))
}
	
setMethod("rownames",
	signature(x = "Vegsoup", do.NULL = "missing", prefix = "missing"),
	function (x) {
		unique(species(x)$plot)
	}
)

".replace.rownames" <- function (x, value) {	
	if (length(value) != nrow(x)) {
		stop("length of values must match nrow(x)", call. = FALSE)
	}
	xy <- list(x = rownames(sites(x)), y = value)
	
	#	species	
	pl <- factor(species(x)$plot, ordered = FALSE)
	sel <- match(levels(pl), xy$x)
	xy$x <- xy$x[sel]
	xy$y <- xy$y[sel]
	levels(pl) <- xy$y
	x@species$plot <- as.character(pl)
	
	#	sites
	sel <- match(rownames(sites(x)), xy$x)
	rownames(x@sites) <- xy$y[sel]

	#	points
	sel <- match(x@sp.points$plot, xy$x)
	x@sp.points$plot <- as.character(xy$y[sel])
	row.names(x@sp.points) <- x@sp.points$plot

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
		a <- species(x)$abbr
		l <- species(x)$layer
		al <- sprintf("%s@%s", a, l)
		res <- unique(unlist(sapply(layers(x), function (x) al[l == x])))
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

setMethod("dimnames",
	signature(x = "Species"),
	function (x) {
		p <- species(x)$plot
		a <- species(x)$abbr
		l <- species(x)$layer
		al <- sprintf("%s@%s", a, l)
		return(list(p, al))
	}
)

#	for sites data frame
setMethod("names",
	signature(x = "Vegsoup"),
	function (x) {
		names(sites(x))
	}
)

setReplaceMethod("names",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {
		if (length(names(x)) != length(value)) {
			stop("length of value must match length names(x)", call. = FALSE)
		}
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
		row.names(sites(x))	
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

decode <- function (x, obj) {
	if (!(inherits(obj, "Vegsoup") | inherits(obj, "Taxonomy"))) {
		stop("argument obj must inherit from classes Vegsoup or Taxonomy")
	}

	if (inherits(x, "matrix") | inherits(x, "data.frame")) {
		x <- rownames(x)
	} else {
		if (inherits(x, "character") | inherits(x, "list")) {
			if (inherits(x, "list")) x <- names(x)
		} else {
		stop("argument not of class, matrix, vector ort list")
		}
	}	
	
	if (length(grep("@", x)) == length(x)) {
		a <- sapply(strsplit(x, "@", fixed = TRUE), "[[" , 1) # abbreviation
		l <- sapply(strsplit(x, "@", fixed = TRUE), "[[" , 2) # layer
	} else {
		message("layer idientifier '@' not found, or unconsistent")
		a <- x
		l <- rep(NA, length(a)) 
	}
	t <- taxonomy(obj)$taxon[match(a, taxonomy(obj)$abbr)]
	r <- list(abbr = a, layer = l, taxon = t)

	return(r)
}

#	convert abbr to taxon names
#if (!isGeneric("splitAbbr")) {
setGeneric("splitAbbr",
	function (obj)
		standardGeneric("splitAbbr")
)
#}
setMethod("splitAbbr",
	signature(obj = "Vegsoup"),
	function (obj) {
		al <- colnames(obj)
		r <- decode(al, obj)
		r <- as.data.frame(r, stringsAsFactors = FALSE, row.names = al)

		if (any(is.na(r$layer)) | any(is.na(r$taxon))) {
			stop("\n unable to deparse layer string,",
				" consider setting type to nospace", call. = FALSE)
		}
		return(invisible(r))
		}
)

#setMethod("abbr",
#   signature(obj = "Vegsoup"),
#	function (obj) {
#		splitAbbr(obj)$abbr
#	}
#)

setMethod("abbr",
	signature(obj = "Vegsoup"),
	function (obj) {
		sort(unique(species(obj)$abbr))
	}
)

#if (!isGeneric("taxon")) {
#	get or set taxon abbreviation
setGeneric("taxon",
	function (obj) {
		standardGeneric("taxon")
	}	
)
#}
setMethod("taxon",
	signature(obj = "Vegsoup"),
	function (obj) {
		taxonomy(obj)$taxon
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