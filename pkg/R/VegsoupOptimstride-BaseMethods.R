setMethod("getK",
	signature(x = "VegsoupOptimstride"),
	function (x) {
		x@optimstride$settings$args$k
	}	
)

setGeneric("methods",
	function (x, ...)
		standardGeneric("methods")
)

setMethod("methods",
    signature(x = "VegsoupOptimstride"),
    function (x, ...) {
    	x@optimstride$settings$args$method
})  

setGeneric("stride",
	function (x, method, ...)
		standardGeneric("stride")
)

setMethod("stride",
    signature(x = "VegsoupOptimstride"),
    function (x, method, ...) {
    	if (missing(method)) {
    		method <- methods(x)#[1]
    	}
    	r <- x@optimstride$indicators
    	m <- match(method, names(r))
    	if (any(is.na(m))) stop("method ", m[is.na(m)], " not found")
    	if (length(m) > 1) {
    		return(r[m])	
    	}
    	else {
    		return(r[[m]])
    	}
    	
    	    	
}) 

setGeneric("treshold",
	function (x, ...)
		standardGeneric("treshold")
)


setMethod("treshold",
    signature(x = "VegsoupOptimstride"),
    function (x, ...) {
    	x@optimstride$settings$args$ft.treshold
    }
)

setGeneric("optimclass1",
	function (x, ...)
		standardGeneric("optimclass1")
)

setMethod("optimclass1",
    signature(x = "VegsoupOptimstride"),
    function (x, ...) {
    	i <- x@optimstride$indicators
       	t(sapply(i, function (x) sapply(x, function (y) sum(y))))
    }
)

setGeneric("optimclass2",
	function (x, treshold = 2, ...)
		standardGeneric("optimclass2")
)

setMethod("optimclass2",
    signature(x = "VegsoupOptimstride"),
    function (x, treshold, ...) {
    	i <- x@optimstride$indicators
       	t(sapply(i, function (x) sapply(x, function (x) length(which(x >= treshold)))))
    }
)

#	from base
setMethod("which.max",
    signature(x = "VegsoupOptimstride"),
    function (x) {    	
    	s <- sapply(stride(x), function (y) sapply(y, sum))
    	apply(s, 2, which.max)
    }
)

.peaks <- function (x) {	
	.turnpoints <- function (x) {
		# insirped from function turnpoints by Frédéric Ibanez in library pastecs
	
	    n <- length(x)
	    #	differences
	    d <- c(x[1] - 1, x[1:(n - 1)]) != x
	    #	uniques values 
	    u <- x[d]
	    #	length of unique values
	    n2 <- length(u)
	    #	positions
	    p <- (1:n)[d]
	    #	ex aequo points
	    e <- c(p[2:n2], n + 1) - p - 1
	
	    m <- n2 - 2
	    em <- matrix(u[1:m + rep(3:1, rep(m, 3)) - 1], m)
	    
	    peaks <- c(FALSE, apply(em, 1, max, na.rm = TRUE) == em[, 2], FALSE)
	    pits <- c(FALSE, apply(em, 1, min, na.rm = TRUE) == em[, 2], FALSE)
	
	    res <- c(p + e)[peaks]
	    return(res)
	}
	
	if (getK(x) <= 3) {
		stop("no calculation for k less than 3")
	}

	s <- sapply(stride(x), function (y) sapply(y, sum))
	res <- apply(s, 2, .turnpoints)
	return(res)
}

setGeneric("peaks",
	function (x, ...)
		standardGeneric("peaks")
)

setMethod("peaks",
    signature(x = "VegsoupOptimstride"),
    function (x, ...) {
    	.peaks(x)
    }
)

#	generating function
#setMethod("print",
#    signature(object = "VegsoupOptimstride"),
#    function (object) {
#			print(object@optimstride)
#    }
#)

#if (!isGeneric("plot")) {
#	setGeneric("plot", function(x, y, ...)
#		standardGeneric("plot"))
#}	