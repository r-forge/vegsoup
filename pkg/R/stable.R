#	adopted for vegsoup from script Classification Stability supplied for JUICE
#	http://www.sci.muni.cz/botany/juice/R/classification%20stability.r

.stable <- function (x, nitr = 200, nitr.lambda = 10, ...)	{
	
	k = getK(x)
	
	pb <- txtProgressBar (min = 1, max = nitr, char = ".",
		width = 45, style = 3)
	
	#setTxtProgressBar (pb, 1)
	   
	r.lambda <- 0
	lambda <- vector(mode = "numeric", length = nitr)
			
	for (i in 1:nitr) {
		setTxtProgressBar (pb, i)
	#	subset selection
	#	suppressWarnings
		sub <- SampleVegsoup(x, replace = TRUE)
			
	#	subset classification
		X <- VegsoupPartition(sub, k = k, method = x@method , ...)
		Y <- x[rownames(x) %in% rownames(X), ]
			
	#	raw lambda calculation
		lambda[i] <- Confus(X, Y)$lambda
		
	#	random lambda calculation
		r.nitr.lambda <- sapply(1:nitr.lambda, function (x) {
			a <- factor(sample(1:k, nrow(X), replace = TRUE), levels = 1:k)
	   		b <- factor(sample(1:k, nrow(X), replace = TRUE), levels = 1:k)
	   		m <- table(a, b)
	   		q1 <- sum(apply(m, 1, function(x) max(x)))
	   		q2 <- sum(apply(m, 2, function(x) max(x)))
	   		q3 <- max(rowSums(m))
	   		q4 <- max(colSums(m))
	   		q5 <- 2 * sum(m)
	   		lambda <- (q1 + q2 - q3 - q4) / (q5 - q3 - q4)
	   		return(lambda)	
	 	})
	 	r.lambda <- r.lambda + sum(r.nitr.lambda)
	}
	
	#	final modified lambda calculation	
	lambda <- sum(lambda) / nitr
	r.lambda <- r.lambda / (nitr * nitr.lambda)
	mod.lambda <- (lambda - r.lambda) / (1 - r.lambda)
	
	res <- list(lambda = lambda, modified.lambda = mod.lambda)
	return(res)
}


#if (!isGeneric("Optsil"))) {
setGeneric("stable",
	function (x, nitr = 200, nitr.lambda = 10, ...)
		standardGeneric("stable")
)
#}

setMethod("stable",
    signature(x = "VegsoupPartition"),
	.stable
)    	
