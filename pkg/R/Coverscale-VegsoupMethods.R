#	based on Coverscale-methods 
setMethod("is.continuous",
    signature(x = "Vegsoup"),
    function (x) {
  		is.continuous(coverscale(x))   	
    }
)

setMethod("is.ordinal",
    signature(x = "Vegsoup"),
    function (x) {
  		is.ordinal(coverscale(x))   	
    }
)

setMethod("coverscale",
    signature(x = "Vegsoup"),
    function (x) {
  		x@coverscale   	
    }
)

#	needs cover scale conversion 
setReplaceMethod("coverscale",
	signature(x = "Vegsoup", value = "Coverscale"),
	function (x, value) {
#		x <- coenoflex(100,100)
#		value <- Coverscale("ordinal")			
		transform <- is.continuous(x) & is.ordinal(value)
		x@coverscale <- value			
		if (transform) {
			x <- as.numeric(Species(x)$cov) # as long as we store characters
			if (max(x) > 100) {
				stop("highest cover value is bigger than 100")
			}
			#	move lowest value into range
			x[x < coverscale(x)@lims[1]] <- coverscale(x)@lims[1]
			
			x@species$cov <- as.character(
				cut(x, 
					breaks = c(coverscale(x)@lims, 100),
					labels = coverscale(x)@codes,
					include.lowest = TRUE))					
			message("transformed cover values!")
		}
		test <- any(is.na(factor(Species(x)$cov, # was !any
			levels = coverscale(x)@codes,
			labels = coverscale(x)@lims)))
		if (test) {
			stop("coverscale does not match data", call. = FALSE)
		}		
		return(x)		
	}
)
setReplaceMethod("coverscale",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {		
		COVERSCALES <- names(.COVERSCALES) # defined in Class-Coverscale.R         
       	value <- match.arg(value, COVERSCALES, several.ok = TRUE)		
		value <- as(.COVERSCALES[[match.arg(value, COVERSCALES)]], "Coverscale")		
		transform <- is.continuous(coverscale(x)) & is.ordinal(value)
		x@coverscale <- value			
		
		if (transform) {
			x <- as.numeric(Species(x)$cov) # as long as we store characters
			if (max(x) > 100) {
				stop("highest cover value is bigger than 100")
			}

			#	move lowest value into range
			x[x < coverscale(x)@lims[1]] <- coverscale(x)@lims[1]
			
			x@species$cov <- as.character(
				cut(x,
					breaks = c(coverscale(x)@lims, 100),
					labels = coverscale(x)@codes,
					include.lowest = TRUE))
			message("transformed cover values!")
		}
		test <- any(is.na(factor(Species(x)$cov,
			levels = coverscale(x)@codes,
			labels = coverscale(x)@lims)))
		if (test) {
			stop("coverscale does not match data", call. = FALSE)
		}		
		return(x)		
	}
)
  
#	revert abunace scale for Braun-Blanquet scale
".BraunBlanquetReduce" <-  function (x) {

	res <- Species(x)
	for (i in c("2m", "2a", "2b")) {
		if (i == "2m")
			res$cov[res$cov == i]  <- "1"
		if (i == "2a")
			res$cov[res$cov == i]  <- "2"
		if (i == "2b")
			res$cov[res$cov == i]  <- "2"
	}
	
	x@species <- res
	x@coverscale <- Coverscale("braun.blanquet2")

	return(invisible(x))
}

#if (!isGeneric("BraunBlanquetReduce"))
setGeneric("BraunBlanquetReduce",
	function (x)
		standardGeneric("BraunBlanquetReduce")
)
setMethod("BraunBlanquetReduce",
    signature(x = "Vegsoup"),
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

