#	get slot
setGeneric("sites",
	function (obj, ...)
		standardGeneric("sites")
)
setMethod("sites",
    signature(obj = "Sites"),
    function (obj) obj@data
)
setMethod("sites",
    signature(obj = "data.frame"),
    function (obj) {
    	new("Sites", data = obj)
    }
    
)
setMethod("sites",
    signature(obj = "matrix"),
    function (obj) {
    	new("Sites",
    	data = as.data.frame(obj, stringsAsFactors = FALSE))
    }
    
)
setMethod("sites",
    signature(obj = "character"),
    function (obj, ...) {
    	new("Sites",
    	data = read.csv(obj, ...))
    }
    
)
setMethod("$", "Sites", 
	function(x, name) {
		if (!("data" %in% slotNames(x))) {
			stop("no $ method for object without slot data")
		}
		return(x@data[[name]])
	}
)
setMethod("show",
    signature(object = "Sites"),
    function (object) {
		cat("object of class     :", class(object))
		cat("\nnumber of variables : ",
			length(unique(object$variable)))
		cat("\nnumber of sites     :",
			length(unique(object$plot)))
		cat("\nshow only frist 6 rows\n\n")
		print(head(object@data, n = 6L))
    }
)
".rbind.Sites" <- function (..., deparse.level = 1) {
	allargs <- list(...)
	#allargs <- list(sts, sts.xy)	
	res <- do.call("rbind", lapply(allargs, sites))
	return(sites(res))

}
#	Sites, Taxonomy Vegsoup have also rbind method
if (!isGeneric("rbind")) {
setGeneric("rbind",
		function (..., deparse.level = 1)
		standardGeneric("rbind"),
		signature = "...")
}
setMethod("rbind",
    signature(... = "Sites"),
	.rbind.Sites
)	