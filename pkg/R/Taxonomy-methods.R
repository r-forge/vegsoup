setGeneric("taxonomy",
	function (obj, ...)
		standardGeneric("taxonomy")
)
setMethod("taxonomy",
    signature(obj = "Taxonomy"),
    function (obj) obj@data
)
setMethod("taxonomy",
    signature(obj = "data.frame"),
    function (obj) {
    	new("Taxonomy", data = obj)
    }
    
)
setMethod("taxonomy",
    signature(obj = "matrix"),
    function (obj) {
    	new("Taxonomy",
    	data = as.data.frame(obj, stringsAsFactors = FALSE))
    }    
)
setMethod("taxonomy",
    signature(obj = "character"),
    function (obj, ...) {
    	new("Taxonomy",
    	data = read.csv(obj, ...))
    }    
)
setMethod("show",
    signature(object = "Taxonomy"),
    function (object) {
		cat("object of class :", class(object))
		cat("\nnumber of taxa  :", nrow(object@data))
		cat("\nshow only first",
			ifelse(nrow(object@data) <= 6, nrow(object@data), 6),
			"rows\n\n")
		print(head(object@data, n = 6L))
    }
)
setMethod("[",
    signature(x = "Taxonomy",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = FALSE) {
    	taxonomy(x@data[i, j, ...])
    }
)
setMethod("$", "Taxonomy", 
	function(x, name) {
		if (!("data" %in% slotNames(x))) {
			stop("no $ method for object without slot data")
		}
		return(x@data[[name]])
	}
)
".rbind.Taxonomy" <- function (..., deparse.level = 1) {
	allargs <- list(...)	
	res <- do.call("rbind", lapply(allargs, taxonomy))
	
	if (!length(unique(res$abbr)) == nrow(res)) {
		message("intersecting taxa abbreviations ('abbr') found.",
			" Drop what is doubled!")
		res <- unique(res)
	}
	return(taxonomy(res))
}
#	Sites, Taxonomy Vegsoup have also rbind method
if (!isGeneric("rbind")) {
setGeneric("rbind",
		function (..., deparse.level = 1)
		standardGeneric("rbind"),
		signature = "...")
}
setMethod("rbind",
    signature(... = "Taxonomy"),
	.rbind.Taxonomy
)	    	
