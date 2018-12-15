#	internal validity check
.validityCoverscale <- function (obj) {
	test <- is.na(
		factor(species(obj)$cov,
		levels = coverscale(obj)@codes,
		labels = coverscale(obj)@lims))
	if (any(test)) FALSE else TRUE
	#stop("coverscale does not match data", call. = FALSE)	 				
}			

#	internal function
#	if just slot 'name' differs
.identicalCoverscale <- function (x, y) {
	test1 <- length(x@codes) == length(y@codes)
	test2 <- length(x@lims) == length(y@lims)
	if (test1 & test2)
		if (all(x@codes == y@codes) & all(x@lims == y@lims)) TRUE else FALSE
	else
		FALSE
}

Coverscale <- function (name, codes, lims) {
	if (missing(name)) {
		cat("buitin coverscales are:\n",
		paste(names(.COVERSCALES), collapse = "\n"))
	}
	else {
		if (missing(codes) & missing(lims)) {
			r <- .COVERSCALES[[match.arg(name, names(.COVERSCALES))]]
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
					r <- list(
						name = as.character(name),
						codes = as.character(codes),
						lims = as.numeric(lims))	
				}
			}
		}		
	r <- as(r, "Coverscale")
	
	#	order to lims
	if (! is.null(r@lims)) {		
		i <- order(r@lims)
		r@codes <- r@codes[i]
		r@lims <- r@lims[i]
	}
	return(r)
	}
}

#	vegan defines:
#	coverscale(x, scale=c("Braun.Blanquet", "Domin", "Hult", "Hill",
#	"fix","log"), maxabund, character = TRUE)


#if (!isGeneric("coverscale")) {
setGeneric("coverscale",
	function (x, scale = c("Braun.Blanquet", "Domin", "Hult", "Hill",
	"fix","log"), maxabund, character = TRUE)
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
#	function (object) {
#			print(paste("cover scale:", object@name))
#			print(rbind(codes = object@codes, lims = object@lims), quote = FALSE)
#	}
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

setMethod("is.occurence",
	signature(x = "Vegsoup"),
	function (x) {
  		is.occurence(coverscale(x))
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

		ss <- .identicalCoverscale(coverscale(x), value) # same, same
		pa <- is.occurence(value)						 # presence/absence
		bb <- coverscale(x)@name == "Braun-Blanquet" & value@name == "Braun-Blanquet 2"		
		oo <- is.ordinal(x) & is.ordinal(value)
		co <- is.continuous(x) & is.ordinal(value)
		
		if (ss)	x@coverscale@name <- value@name
		
		if (pa) {
			species(x)$cov <- "1"
			x@coverscale <- value
		}

		if ( (oo & !bb) & (oo | co) & !ss & !pa ) {
			#	not implemented yet
		}
			
		if ( (oo & bb) & !ss & !pa ) {
			r <- species(x) #! slot data
			for (i in c("2m", "2a", "2b")) {
				if (i == "2m") r$cov[r$cov == i]  <- "1"
				if (i == "2a") r$cov[r$cov == i]  <- "2"
				if (i == "2b") r$cov[r$cov == i]  <- "2"
			}
			species(x) <- r
			x@coverscale <- value
		}					
		
		if ( (oo | co) & !ss & !bb & !pa ) {
			xx <- as.numeric(species(x)$cov) # we store characters

			if (max(xx) > 100) stop("highest cover value is bigger than 100")

			#	move lowest value into range
			xx[xx < value@lims[1]] <- value@lims[1]
								
			x@species$cov <- as.character(
				cut(xx, 
					breaks = c(value@lims, 100),
					labels = value@codes,
					include.lowest = TRUE, right = FALSE))
					
			x@coverscale <- value

			test <- .validityCoverscale(x)
			if (test)
				message("transformed cover values!")
			else
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
		
		coverscale(x) <- value
			
		return(x)		
	}
)
  
#if (!isGeneric("BraunBlanquetReduce")) {
setGeneric("BraunBlanquetReduce",
	function (x)
		standardGeneric("BraunBlanquetReduce")
)
#}

setMethod("BraunBlanquetReduce",
	signature(x = "Vegsoup"),
	function (x) {	
		coverscale(x) <- Coverscale("braun.blanquet2")
		return(x)
	}
)
