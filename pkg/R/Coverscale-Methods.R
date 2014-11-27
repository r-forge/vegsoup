#	Vegsoup validity check
#	!any(is.na(factor(x$cov, levels = scale$codes, labels = scale$lims)))
#	x$cov <- as.numeric(x$cov)
#	if (any(is.na(x$cov))) {
#	str(x$cov)
#	stop("there seems to be digits mixed with characters?")

Coverscale <- function (name, codes, lims) {
	if (missing(name)) {
		cat("buitin coverscales are:\n",
		paste(names(.COVERSCALES), collapse = "\n"))
	}
	else {
		if (missing(codes) & missing(lims)) {
			res <- .COVERSCALES[[match.arg(name, names(.COVERSCALES))]]
		}
		else {
			if (missing(codes) | missing(lims))	{
				stop("need both codes and lims", call. = FALSE)
			}
			else {
				if (length(codes) != length(lims)) {
					stop("length of codes and lims are not the same", call. = FALSE)
				}
				else {
					res <- list(
						name = as.character(name),
						codes = as.character(codes),
						lims = as.numeric(lims))	
				}
			}
		}
	res <- as(res, "Coverscale")
	return(res)
	}
}

#	vegan defines:
#	coverscale(x, scale=c("Braun.Blanquet", "Domin", "Hult", "Hill",
#	"fix","log"), maxabund)

#if (!isGeneric("coverscale")) {
setGeneric("coverscale",
	function (x, scale = c("Braun.Blanquet", "Domin", "Hult", "Hill",
	"fix","log"), maxabund)
	standardGeneric("coverscale"))
#}

#if (!isGeneric("coverscale <-")) {
setGeneric("coverscale<-",
	function (x, value)
		standardGeneric("coverscale<-")
)
#}

#if (!isGeneric("is.ordinal")) {
setGeneric("is.ordinal", function (x)
	standardGeneric("is.ordinal"))
#}

#if (!isGeneric("is.continuous")) {
setGeneric("is.continuous", function (x)
	standardGeneric("is.continuous"))
#}

#if (!isGeneric("is.occurence")) {
setGeneric("is.occurence", function (x)
	standardGeneric("is.occurence"))
#}

setMethod("is.ordinal",
    signature(x = "Coverscale"),
    function (x) {
  		!is.null(x@codes) & !is.null(x@codes)
    }
)

setMethod("is.continuous",
    signature(x = "Coverscale"),
    function (x) {
  		is.null(x@codes) & is.null(x@codes)
    }
)

setMethod("is.occurence",
    signature(x = "Coverscale"),
    function (x) {
  		x@name == "pa"
    }
)

#setMethod("show",
#  signature(object = "Coverscale"),
#    function (object) {
#			print(paste("cover scale:", object@name))
#			print(rbind(codes = object@codes, lims = object@lims), quote = FALSE)
#    }
#)
#	removeMethod("show", "Coverscale")

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
		
		transform <- is.continuous(x) & is.ordinal(value) | is.occurence(value)
		x@coverscale <- value
					
		if (transform) {
			if (is.occurence(value)) {
				x@species$cov <- as.character(1)
				message("transformed cover values!")
			}
			else {
				x <- as.numeric(species(x)$cov) # we store characters
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
			test <- any(is.na(
				factor(species(x)$cov,
				levels = coverscale(x)@codes,
				labels = coverscale(x)@lims)))

			if (test) stop("coverscale does not match data", call. = FALSE)
			}			
		return(x)
		}
	}
)

setReplaceMethod("coverscale",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {		
		COVERSCALES <- names(.COVERSCALES) # defined in Class-Coverscale.R         
       	value <- match.arg(value, COVERSCALES, several.ok = TRUE)		
		value <- as(.COVERSCALES[[match.arg(value, COVERSCALES)]], "Coverscale")
		
		coverscale(x) <- value
			
		return(x)		
	}
)
  
#	revert abundance scale for Braun-Blanquet scale
#if (!isGeneric("BraunBlanquetReduce")) {
setGeneric("BraunBlanquetReduce",
	function (x)
		standardGeneric("BraunBlanquetReduce")
)
#}

setMethod("BraunBlanquetReduce",
    signature(x = "Vegsoup"),
	function (x) {	
		res <- species(species(x)) #! slot data
		for (i in c("2m", "2a", "2b")) {
			if (i == "2m")
				res$cov[res$cov == i]  <- "1"
			if (i == "2a")
				res$cov[res$cov == i]  <- "2"
			if (i == "2b")
				res$cov[res$cov == i]  <- "2"
		}
		#! now will also work species(obj) <- species(res)
		x@species <- species(res)
		coverscale(x) <- Coverscale("braun.blanquet2")
		return(invisible(x))
	}
)
