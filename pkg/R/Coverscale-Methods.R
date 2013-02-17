#	Vegsoup validity check
#	!any(is.na(factor(x$cov, levels = scale$codes, labels = scale$lims)))
#x$cov <- as.numeric(x$cov)
#if (any(is.na(x$cov))) {
#str(x$cov)
#stop("there seems to be digits mixed with characters?")

Coverscale <- function (name, codes, lims) {
	if (missing(name)) {
		cat("buitin coverscales are:\n",
		paste(names(.COVERSCALES), collapse = "\n"))
	} else {
	if (missing(codes) & missing(lims)) {
		res <- .COVERSCALES[[match.arg(name, names(.COVERSCALES))]]
	} else {
		if (missing(codes) | missing(lims))	{
			stop("need both codes and lims", call. = FALSE)
		} else {
			if (length(codes) != length(lims)) {
				stop("length of codes and lims are not the same", call. = FALSE)
			} else {
				res <- list(
					name = as.character(name),
					codes = as.character(codes),
					lims = as.numeric(lims)
					)	
			}
		}
	}
	res <- as(res, "Coverscale")
	return(res)
	}
}

#if (!isGeneric("coverscale")) {
setGeneric("coverscale", function(obj, ...)
	standardGeneric("coverscale"))
#}
#if (!isGeneric("coverscale <-")) {
setGeneric("coverscale<-",
	function (obj, value, ...)
		standardGeneric("coverscale<-")
)
#}
#if (!isGeneric("coverscale")) {
setGeneric("is.ordinal", function(obj, ...)
	standardGeneric("is.ordinal"))
#}
#if (!isGeneric("coverscale")) {
setGeneric("is.continuous", function(obj, ...)
	standardGeneric("is.continuous"))
#}
setMethod("coverscale",
    signature(obj = "Vegsoup"),
    function (obj) {
  		obj@coverscale   	
    }
)
setMethod("is.ordinal",
    signature(obj = "Coverscale"),
    function (obj) {
  		!is.null(obj@codes) & !is.null(obj@codes)
    }
)
setMethod("is.continuous",
    signature(obj = "Coverscale"),
    function (obj) {
  		is.null(obj@codes) & is.null(obj@codes)
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
				cut(Species(obj)$cov,
					breaks = c(coverscale(obj)@lims, 100),
					labels = coverscale(obj)@codes))
			warning("transformed cover values", .call = FALSE)					
		}		
		test <- any(is.na(factor(Species(obj)$cov, # was !any
			levels = coverscale(obj)@codes,
			labels = coverscale(obj)@lims)))
		if (test) {
			stop("coverscale does not match data", .call = FALSE)
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
				cut(Species(obj)$cov,
					breaks = c(coverscale(obj)@lims, 100),
					labels = coverscale(obj)@codes))
			warning("transformed cover values", .call = FALSE)					
		}
		test <- any(is.na(factor(Species(obj)$cov, # was !any
			levels = coverscale(obj)@codes,
			labels = coverscale(obj)@lims)))
		if (test) {
			stop("coverscale does not match data")
		}		
		return(obj)		
	}
)
  
setAs("list", "Coverscale", def = function (from) {
	#	ordinal
	if (!is.null(from[[2]]) & !is.null(from[[3]])) {
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = as.character(from[[2]]),
			lims = as.numeric(from[[3]])						
			)
	}		
	#	continous
	if (is.null(from[[2]]) & is.null(from[[3]])) { # 			
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = NULL,
			lims = NULL						
			)			
	}
	return(res)
})

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

