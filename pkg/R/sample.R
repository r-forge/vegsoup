#	sample data, usally without replacement
#if (!isGeneric("SampleVegsoup")) {
setGeneric("SampleVegsoup", function (x, size, replace = FALSE, prob = NULL)
	standardGeneric("SampleVegsoup"))
#}
#	warning: does not behave as expected for the user
#	StablePartition() relies on this method
#	to do: documentation
#	think about a method for class (VegsoupPartition) to sample conditional on Partitioning(obj)
setMethod("SampleVegsoup",
    signature(x = "Vegsoup"),
    function (x, size, replace = FALSE, prob = NULL) {
    	#	for sample the default for size is the number of items inferred from the first argument
		if (missing(size)) {
            size <- dim(x)[1]
        }    
		sel <- sample(1:dim(x)[1], size, replace = replace, prob = prob)
    	if (any(table(sel) > 1)) {
    		sel <- sort(unique(sel))
    		warning("\n replace was set to ", replace,
    			", can only select unique plots! A subsample will be returend!", call. = FALSE)
    	}
    	res <- x[sel, ]
    	return(invisible(res))
    }
)