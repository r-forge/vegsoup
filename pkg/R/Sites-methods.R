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
    	data = read.csv(obj, ...)[, 1:3])
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

setReplaceMethod("$",
	signature(x = "Sites"),
	function (x, name, value) {
 		x@data[[name]] <- value 	
		return(x)		
	}
)

#if (isGeneric("variable")) {
setGeneric("variable",
	function (obj, name, ...)
		standardGeneric("variable")
)
#}

setMethod("variable",
    signature(obj = "Sites"),
    function (obj, name) {
    	p <- unique(obj$plot)
    	n <- length(p)
    	i <- which(obj$variable == name)
		r <- structure(obj$value[i], names = obj$plot[i])
		
		#	return NULL if variable is not present
		if (length(r) == 0) return(NULL)
		
		if (length(r) != n) {
			x <- structure(rep(NA, n), names = p)
			x[names(r)] <- r
			r <- x
		}
		return(r)
    }
)

setMethod("[",
    signature(x = "Sites",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = FALSE) {
		if (!missing(j)) message("ignore argument j")
    	j <- rep(TRUE, ncol(sites(x)))    	
    	sites(x@data[i, j, ...])
    }
)

setMethod("show",
    signature(object = "Sites"),
    function (object) {
		cat("object of class     :", class(object))
		cat("\nnumber of variables :",
			length(unique(object$variable)))
		cat("\nnumber of sites     :",
			length(unique(object$plot)))
		cat("\nshow only frist 6 rows\n\n")
		print(head(object@data, n = 6L))
    }
)

setMethod("rbind",
    signature(... = "Sites"),
	function (..., deparse.level = 1) {
		allargs <- list(...)
		res <- do.call("rbind", lapply(allargs, sites))
		return(sites(res))
	}
)	