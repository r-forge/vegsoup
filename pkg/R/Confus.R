#if(!isGeneric("Confus")) {
setGeneric("Confus",
	function (obj1, obj2)
		standardGeneric("Confus")
)
#}

setMethod("Confus",
    signature(obj1 = "VegsoupPartition",
    	obj2 = "VegsoupPartition"),
	function (obj1, obj2) {

	if (getK(obj1) != getK(obj2)) {
		stop("Numbers of k differ for obj1 (", getK(obj1), ") ",
			"and obj2 (", getK(obj1), ")!", sep = "")
	}
	#	reference (observed) as row margins, compoarison (predicted) as column margins
    	N <- length(Partitioning(obj1))
	    nc <- getK(obj1)
	        	
		res <- table(Partitioning(obj1), Partitioning(obj2))
		
    	correct <- sum(diag(res))
	    percent <- correct / N
		sum <- sum(rowSums(res) * colSums(res))
		
		#	calculate kappa
	    kappa <- (c(N * correct) - sum) / (N^2 - sum)
	    
	    #	warning: formula needs to be confirmed 	
	    #	calculate lambda
		q1 <- sum(apply(res, 1, function (x) max(x)))
		q2 <- sum(apply(res, 2, function (x) max(x)))
		q3 <- max(rowSums(res))
		q4 <- max(colSums(res))
		q5 <- 2 * sum(res)
		lambda <- (q1 + q2 - q3 - q4) / (q5 - q3 - q4)    
	    
    	res <- list(confus = res,
    		correct = correct,
        	kappa = kappa,
        	lambda = lambda)
	    return(res)
	}
)