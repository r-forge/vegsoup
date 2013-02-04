#	Vegsoup validity check
#	!any(is.na(factor(x$cov, levels = scale$codes, labels = scale$lims)))
#x$cov <- as.numeric(x$cov)
#if (any(is.na(x$cov))) {
#str(x$cov)
#stop("there seems to be digits mixed with characters?")

Coverscale <- function (name, codes, lims) {
	if (missing(name)) {
		warning("buitin coverscales are:\n",
		paste(names(.COVERSCALES), collapse = "\n"),
		call. = FALSE)
	}
	if (missing(codes) & missing(lims)) {
		res <- .COVERSCALES[[match.arg(name, names(.COVERSCALES))]]
	} else {
		if (missing(codes) | missing(lims))	{
			stop("need both codes and lims", call. = FALSE)
		} else {
			if (length(codes) != length(lims)) {
				stop("length of codes and lims must", call. = FALSE)
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

#if (!isGeneric("coverscale")) {
setGeneric("coverscale", function(obj, ...)
	standardGeneric("coverscale"))
#}
#if (!isGeneric("decostand<-")) {
setGeneric("coverscale<-",
	function (obj, value, ...)
		standardGeneric("coverscale<-")
)
#}
setMethod("coverscale",
    signature(obj = "Vegsoup"),
    function (obj) {
 		obj@coverscale # rename to coverscale   	
    }
)

#	needs cover scale conversion 
setReplaceMethod("coverscale",
	signature(obj = "Vegsoup", value = "Coverscale"),
	function (obj, value) {		
		obj@coverscale <- value
		test <- !any(is.na(factor(Species(obj)$cov,
			levels = coverscale(obj)@codes,
			labels = coverscale(obj)@lims)))
		if (test) {
			stop("coverscale does not match data")
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
		obj@coverscale <- value
		test <- !any(is.na(factor(Species(obj)$cov,
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


#setMethod("show",
#  signature(object = "Coverscale"),
#    function (object) {
#			print(paste("cover scale:", object@name))
#			print(rbind(codes = object@codes, lims = object@lims), quote = FALSE)
#    }
#)
#	removeMethod("show", "Coverscale")

