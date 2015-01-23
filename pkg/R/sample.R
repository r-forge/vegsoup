#	sample data, usally without replacement
#if (!isGeneric("SampleVegsoup")) {
setGeneric("SampleVegsoup", function (x, size, replace = FALSE, prob = NULL)
	standardGeneric("SampleVegsoup"))
#}
#	warning: does not behave as expected for the user
#	StablePartition() relies on this method
#	think about a method for class (VegsoupPartition) to sample conditional on Partitioning(obj)
setMethod("SampleVegsoup",
    signature(x = "Vegsoup"),
    function (x, size, replace = FALSE, prob = NULL) {
    	#	for sample the default for size is the number of items inferred from the first argument
		N <- nrow(x)
		if (missing(size)) {
            size <- N
        }    
		i <- sample(1:N, size, replace = replace, prob = prob)
    	if (any(table(i) > 1)) {
    		i <- sort(unique(i))
    		#message("\nreplace was set to ", replace,
    		#	", can only select unique plots! A subsample will be returend!")
    	}
    	r <- x[i, ]
    	return(r)
    }
)

#	Heterogeneity-constrained random samples
#if (!isGeneric("SampleVegsoup")) {
setGeneric("hcr", function (x, size, nperm = 1000, fast = FALSE, ...)
	standardGeneric("hcr"))
#}

setMethod("hcr",
    signature(x = "Vegsoup"),
	function (x, size, nperm = 1000, fast = FALSE, ...) {

	if (as.logical(fast)) {
		#	parallel is in imports
		message("fork multicore process on ", parallel::detectCores(), " cores")
	}			
	
	N <- nrow(x)
    X <- as.matrix(as.dist(x))
    #X <- as.dist(x)
    
    if (missing(size)) {
    	size <- floor(nrow(x) * 0.1)	
    }
    if (size > N) {
        stop("size can never exceed number of plots")
    }
    
    #	return mean, variance and sample indices for permuations
    if (fast) {
	    s <- mclapply(1:nperm, function (x) {
			    i <- sample(x = N, size = size, ...)
		        x <- as.vector(as.dist(X[i, i]))
		        return(list(mean(x), var(x), i))
	    	}, ...)    	
    }
    else {
	    s <- lapply(1:nperm, function (x) {
			    i <- sample(x = N, size = size, ...)
		        x <- as.vector(as.dist(X[i, i]))
		        return(list(mean(x), var(x), i))
	    	})#, ...)    	    	
    }
    #	vector of means
    m <- sapply(s, "[[", 1)
    #	vector of variances
    v <- sapply(s, "[[", 1)
    #	matrix of selected random samples for each permutation
    s <- sapply(s, "[[", 3)    
	#	rank by decreasing mean and increasing variances
    r <- rank(rank(-m) + rank(v))
    #	obtain sample index
    r <- sort(s[, which.min(r)])
    #	return the subset
    return(x[r, ])
}
)