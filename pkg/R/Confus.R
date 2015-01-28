#if(!isGeneric("confusion")) {
setGeneric("confusion",
	function (obj1, obj2)
		standardGeneric("confusion")
)
#}

".confusion" <- function (t, N) {
		stopifnot(is.table(t))
		D <- sum(diag(t))
		P <-  D / N * 100 # percentage correct
		S <- sum(rowSums(t) * colSums(t))
		
		#	calculate kappa
		K <- (c(N * D) - S) / (N^2 - S)

		#	calculate lambda		
		#	formula needs to be confirmed by a reference
		q1 <- sum(apply(t, 1, function (x) max(x)))
		q2 <- sum(apply(t, 2, function (x) max(x)))
		q3 <- max(rowSums(t))
		q4 <- max(colSums(t))
		q5 <- 2 * sum(t)
		L <- (q1 + q2 - q3 - q4) / (q5 - q3 - q4) # lambda
		
		r <- list(confus = t, correct = P, kappa = K, lambda = L)
		
		return(r)
}

setMethod("confusion",
	signature(obj1 = "VegsoupPartition",
		obj2 = "VegsoupPartition"),
	function (obj1, obj2) {

		if (getK(obj1) != getK(obj2))
			stop("Numbers of k differ for obj1 (", getK(obj1), ") ",
				"and obj2 (", getK(obj1), ")!", sep = "")
		stopifnot(all.equal(dim(obj1), dim(obj2)))
		
		#	reference (observed) as row margins, comparison (predicted) as column margins
		N <- length(partitioning(obj1))	
		t <- table(partitioning(obj1), partitioning(obj2))
		
		r <- .confusion(t, N)

		return(r)
	}
)
