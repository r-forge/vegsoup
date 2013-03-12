#	get slot species
setGeneric("species",
	function (obj, ...)
		standardGeneric("species")
)
setMethod("species",
    signature(obj = "Species"),
    function (obj) {
    	obj@data
    }	
)
setMethod("species",
    signature(obj = "data.frame"),
    function (obj) {
    	new("Species", data = obj)
    }
    
)
setMethod("species",
    signature(obj = "matrix"),
    function (obj) {
    	new("Species",
    	data = as.data.frame(obj, stringsAsFactors = FALSE))
    }
    
)
setMethod("species",
    signature(obj = "character"),
    function (obj, ...) {
    	new("Species",
    	data = read.csv(obj, ...))
    }
    
)
setMethod("show",
    signature(object = "Species"),
    function (object) {
		cat("object of class   :",
			class(object))
		cat("\nnumber of species :",
			length(unique(species(object)$abbr)))
		cat("\nnumber of sites   :",
			length(unique(species(object)$plot)))		
		cat("\nshow only first",
			ifelse(nrow(object@data) <= 6, nrow(object@data), 6),
			"rows\n\n")
		print(head(object@data, n = 6L))
    }
)
setMethod("[",
    signature(x = "Species",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = FALSE) {
    	species(x@data[i, j, ...])
    }
)
setMethod("$", "Species", 
	function(x, name) {
		if (!("data" %in% slotNames(x))) {
			stop("no $ method for object without slot data")
		}
		return(x@data[[name]])
	}
)
setReplaceMethod("$",
	signature(x = "Species"),
	function (x, name, value) {
 		x@data[[name]] <- value 	
		return(x)		
	}
)
".rbind.Species" <- function (..., deparse.level = 1) {
	allargs <- list(...)
	#allargs <- list(sts, sts.xy)	
	res <- do.call("rbind", lapply(allargs, species))
	return(species(res))

}
#	Sites, Taxonomy Vegsoup have also rbind method
if (!isGeneric("rbind")) {
setGeneric("rbind",
		function (..., deparse.level = 1)
		standardGeneric("rbind"),
		signature = "...")
}
setMethod("rbind",
    signature(... = "Species"),
	.rbind.Species
)	     	
#setReplaceMethod("Layers",
#	signature(obj = "Species", value = "ANY"),
#	function (obj, value) {
#		if (length(value) != length(Layers(obj))) {
#			stop("length of value does not match length layers of object")
#		}
#		if (any(!Layers(obj) %in% value)) {
#			stop("items of value do not match layers of object",
#				"\n use Layers(obj, collapse = value),",
#				" where layers to be dropped are coded as NA") 
#		}
#		obj@layers <- value
#		return(obj)		
#	}
#)
