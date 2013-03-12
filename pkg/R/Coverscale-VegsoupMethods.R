setMethod("coverscale",
    signature(obj = "Vegsoup"),
    function (obj) {
  		obj@coverscale   	
    }
)

#	needs cover scale conversion 
setReplaceMethod("coverscale",
	signature(obj = "Vegsoup", value = "Coverscale"),
	function (obj, value) {	
		transform <- is.continuous(coverscale(obj)) & is.ordinal(value)
		obj@coverscale <- value			
		if (transform) {
			obj@species$cov <- as.character(
				cut(as.numeric(Species(obj)$cov), # as long as we store characters
					breaks = c(coverscale(obj)@lims, 100),
					labels = coverscale(obj)@codes))
			warning("transformed cover values of object!", call. = FALSE)
		}		
		test <- any(is.na(factor(Species(obj)$cov, # was !any
			levels = coverscale(obj)@codes,
			labels = coverscale(obj)@lims)))
		if (test) {
			stop("coverscale does not match data", call. = FALSE)
		}		
		return(obj)		
	}
)
setReplaceMethod("coverscale",
	signature(obj = "Vegsoup", value = "character"),
	function (obj, value) {
		COVERSCALES <- names(.COVERSCALES) # defined in Class-Coverscale.R         
       	value <- match.arg(value, COVERSCALES, several.ok = TRUE)		
		value <- as(.COVERSCALES[[match.arg(value, COVERSCALES)]], "Coverscale")		
		transform <- is.continuous(coverscale(obj)) & is.ordinal(value)
		obj@coverscale <- value			
		if (transform) {
			obj@species$cov <- as.character(
				cut(as.numeric(Species(obj)$cov), # as long as we store characters 
					breaks = c(coverscale(obj)@lims, 100),
					labels = coverscale(obj)@codes))
			warning("transformed cover values of object!", call. = FALSE)
		}
		test <- any(is.na(factor(Species(obj)$cov, # was !any
			levels = coverscale(obj)@codes,
			labels = coverscale(obj)@lims)))
		if (test) {
			stop("coverscale does not match data", call. = FALSE)
		}		
		return(obj)		
	}
)
  
#	revert abunace scale for Braun-Blanquet scale
".BraunBlanquetReduce" <-  function (obj) {

	res <- Species(obj)
	for (i in c("2m", "2a", "2b")) {
		if (i == "2m")
			res$cov[res$cov == i]  <- "1"
		if (i == "2a")
			res$cov[res$cov == i]  <- "2"
		if (i == "2b")
			res$cov[res$cov == i]  <- "2"
	}
	
	obj@species <- res
	obj@coverscale <- Coverscale("braun.blanquet2")

	return(invisible(obj))
}

#if (!isGeneric("BraunBlanquetReduce"))
setGeneric("BraunBlanquetReduce",
	function (obj)
		standardGeneric("BraunBlanquetReduce")
)
setMethod("BraunBlanquetReduce",
    signature(obj = "Vegsoup"),
    .BraunBlanquetReduce
)

#setMethod("show",
#  signature(object = "Coverscale"),
#    function (object) {
#			print(paste("cover scale:", object@name))
#			print(rbind(codes = object@codes, lims = object@lims), quote = FALSE)
#    }
#)
#	removeMethod("show", "Coverscale")

