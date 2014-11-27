setGeneric("taxonomy",
	function (obj, ...)
		standardGeneric("taxonomy")
)

setGeneric("taxonomy<-",
	function (obj, value)
		standardGeneric("taxonomy<-")
)

setMethod("taxonomy",
    signature(obj = "Taxonomy"),
    function (obj) obj@data
)

setMethod("taxonomy",
    signature(obj = "character"),
    function (obj, ...) {
    	new("Taxonomy",
    	data = read.csv(obj, ...))
    }    
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

setMethod("$",
	signature(x = "Taxonomy"), 
	function(x, name) {
		if (!("data" %in% slotNames(x))) {
			stop("no $ method for object without slot data")
		}
		return(x@data[[name]])
	}
)

setMethod("abbr",
    signature(obj = "Taxonomy"),
    function (obj) {
    	obj$abbr
    }	
)

setMethod("rbind",
    signature(... = "Taxonomy"),
	function (..., deparse.level = 1) {
		allargs <- list(...)	
		r <- do.call("rbind", lapply(allargs, taxonomy))
		r <- unique(r)
		#	explicit ordering!
		r <- r[order(r$abbr), ]
		#	we might find differnt spelling of taxon for the same abbr!
		test <- duplicated(r$abbr)
		if (any(test)) {
			a <- r$abbr[which(test)]
			print(r[r$abbr == a[[1]], ])
			stop("found duplicates in abbr/taxon pair", call. = FALSE)
		}
		rownames(r) <- r$abbr
	
		return(taxonomy(r))
	}
)	    	
