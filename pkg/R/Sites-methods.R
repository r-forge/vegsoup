#	generic is set in sites.R

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
		n <- scan(obj, what = "character", nlines = 1, quiet = TRUE)
		r <- try(new("Sites",
			data = read.csv(obj, ...)[, 1:3]), silent = TRUE)
		
		if (class(r) == "try-error")
			stop("could not read csv file, maybe try another sep argument? ",
				"first line of file is \"", n, "\"")
		else
			return(r)			
	}	
)

setMethod("$",
	signature(x = "Sites"),
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
	function (x, name, ...)
		standardGeneric("variable")
)
#}

setMethod("variable",
	signature(x = "Sites"),
	function (x, name) {
		p <- unique(x$plot)
		n <- length(p)
		i <- which(x$variable == name)
		r <- structure(x$value[i], names = x$plot[i])
		
		#	return NULL if variable is not present
		if (length(r) == 0) return(NULL)
		
		if (length(r) != n) {
			xx <- structure(rep(NA, n), names = p)
			xx[names(r)] <- r
			r <- xx
		}
		return(r)
	}
)

#if (!isGeneric("variable<-")) {
setGeneric("variable<-",
	function (x, name, value)
		standardGeneric("variable<-")
)
#}

setReplaceMethod("variable",
	signature(x = "Sites", name = "character", value = "ANY"),
	function (x, name, value) {
		i <- x$variable == name
		test1 <- !any(i)
		test2 <- length(variable(x, name)) != length(value) & length(value) != 1

		if (test1) stop("variable not found")
		if (test2) stop("length of value must match length variable(x, name)")
				
		x@data[i, 3] <- as.character(value)
		return(x)
	}
)

#if (isGeneric("variable")) {
setGeneric("variables",
	function (x, ...)
		standardGeneric("variables")
)
#}

setMethod("variables",
	signature(x = "Sites"),
	function (x) {
		r <- unique(x$variable)
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
		cat("object of class	 :", class(object))
		cat("\nnumber of variables :",
			length(unique(object$variable)))
		cat("\nnumber of sites	 :",
			length(unique(object$plot)))
		cat("\nshow only frist 6 rows\n\n")
		print(head(object@data, n = 6L))
	}
)

setMethod("bind",
	signature(... = "Sites"),
	function (..., deparse.level = 1) {
		allargs <- list(...)
		res <- do.call("rbind", lapply(allargs, sites))
		return(sites(res))
	}
)	