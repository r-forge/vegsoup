
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

#if (!isGeneric("names<-")) {
#setGeneric("names <-",
#	function (x, value, ...)
#		standardGeneric("Sites<-")
#)
#}
#	replacement method for names

#	equivalent	
#setMethod('names<-', signature(x='Vegsoup'), 
#	function(x, value)  {
#		x@sites@names <- value
#		x
#	}
#)
setReplaceMethod("names",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {
		x@sites@names <- value
		x
	}
)

#	convert abbr to taxon names
#if (!isGeneric("DecomposeNames")) {
setGeneric("DecomposeNames",
	function (obj, ...)
		standardGeneric("DecomposeNames")
)
#}
setMethod("DecomposeNames",
	signature(obj = "Vegsoup"),
	function (obj, verbose = FALSE) {
	#	obj <- dta; type = "nospace"
	if (verbose) {
		cat("Vegsoup standard pattern taxa coding:")
		cat("blanks are dots, '@' seperates abbreviations and layer")
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

#if (!isGeneric("abbr.layer")) {
setGeneric("abbr.layer",
	function (obj, ...)
		standardGeneric("abbr.layer")
)
#}
setMethod("abbr.layer",
    signature(obj = "Vegsoup"),
    function (obj, ...) {
    	file.path(Species(obj)$abbr, Species(obj)$layer, fsep = "@")
    }
)

#if (!isGeneric("Abbreviation")) {
#	get or set taxon abbreviation
setGeneric("Abbreviation",
	function (obj, ...)
		standardGeneric("Abbreviation")
)
#}
setMethod("Abbreviation",
    signature(obj = "Vegsoup"),
    function (obj, ...) DecomposeNames(obj)$abbr
)
setGeneric("Abbreviation<-",
	function (obj, value, ...)
		standardGeneric("Abbreviation<-")
)
setMethod("Abbreviation",
    signature(obj = "Vegsoup"),
    function (obj) sort(unique(Species(obj)$abbr))
)
setReplaceMethod("Abbreviation",
	signature(obj = "Vegsoup", value = "character"),
	function (obj, value) {
		#	to do: needs security for all slots!
		obj@species$abbr <- value		
		return(obj)		
	}
)