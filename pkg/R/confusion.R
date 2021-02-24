if(!isGeneric("confusion")) {
setGeneric("confusion",
	function (obj1, obj2)
		standardGeneric("confusion")
) }

#	compute Goodman and Kruskal's Lambda
".lambda" <- function (x) {
	#	n <- sum(t)
	#	e1 <- sum(apply(t, 1, max), apply(t, 2, max))
	#	e2 <- max(rowSums(t)) + max(colSums(t))
	#	r <- 0.5 * (e1 - e2) / (n - 0.5 * e2)
	#	r <- (e1 - e2) / (n - e2)
		
	n <-  2 * sum(x)
	e1 <- sum(apply(x, 1, max)) + sum(apply(x, 2, max))
	e2 <- max(rowSums(x)) + max(colSums(x))
	r <- (e1 - e2) / (n - e2)
	
	#	r <- (sum(apply(x, 1, max)) + sum(apply(x, 2, max)) - max(rowSums(x)) - max(colSums(x))) /
	#	(n -  max(rowSums(x)) - max(colSums(x)))
	return(r)
}

#	compute overall accuracy, Cohen's Kappa and Goodman and Kruskal's Lamda
".confusion" <- function (t, N) {
	stopifnot(is.table(t))
	D <- sum(diag(t))
	P <- D / N * 100 # percentage correct
	S <- sum(rowSums(t) * colSums(t))
		
	#	calculate Cohen's kappa measure of agreement
	K <- (c(N * D) - S) / (N^2 - S)

	#	calculate Goodman and Kruskal's lambda	
	L <- .lambda(t)			
	r <- list(confusion = t, correct = P, kappa = K, lambda = L)
		
	return(r)
}

setMethod("confusion",
	signature(obj1 = "VegsoupPartition",
		obj2 = "VegsoupPartition"),
	function (obj1, obj2) {

		if (getK(obj1) != getK(obj2))
			stop("Numbers of k differ for obj1 (", getK(obj1), ") ",
				"and obj2 (", getK(obj2), ")!", sep = "")
		stopifnot(all.equal(dim(obj1), dim(obj2)))
		
		#	reference (observed) as row margins, comparison (predicted) as column margins
		N <- length(partitioning(obj1))	
		t <- table(partitioning(obj1), partitioning(obj2))
		
		r <- .confusion(t, N)

		return(r)
	}
)