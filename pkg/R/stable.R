#	Tichý, L. and Chytrý, M. and Šmarda, P. (2011) Evaluating the stability of
#	the classification of community data. Ecography 34:807-813
#	adopted for vegsoup from script Classification Stability supplied for JUICE
#	http://www.sci.muni.cz/botany/juice/R/classification%20stability.r

.stable <- function (x, nitr = 200, nitr.lambda = 10, ...)	{
	
	k <- getK(x)
	o <- partitioning(x)
	
	pb <- txtProgressBar (min = 1, max = nitr, char = ".",
		width = 45, style = 3)
	
	# results vector to store random lamda values
	rl <- 0
	# results vector to store lamda
	l <- vector(mode = "numeric", length = nitr)
	
	for (i in 1:nitr) {
		setTxtProgressBar(pb, i)

		# subsets created by the without-replacement bootstrap
		xi <- vegsoup::sample(x, size = nrow(x), replace = TRUE)		

		#	using .confusion directly
		s <- VegsoupPartition(xi, k = k, method = x@partitioning.method)
		s <- partitioning(s)
		#	only sites present in the sample are considered 		
		os <- o[ match(names(s), names(o)) ]
		l[ i ] <- suppressWarnings(.confusion(table(os, s), length(s))$lambda)

		#	only sites present in the sample are considered 
		#	common rows (sites), extract of intersect may result in k - i partitions
		#	Y <- x[ rownames(x) %in% rownames(xi), ]				
		#	subset classification, use lowered value of k from sample
		#	in case subsetting resulted in k - i partition
		#	X <- VegsoupPartition(xi, k = getK(Y), method = x@partitioning.method)#, ...)
		#	cross tabulation raw lambda
		#	l[ i ] <- confusion(X, Y)$l
		
		#	random values for lambda for given size
		#	see Tichy et al. eq. 2 lambda_rand
		.randomLambda <- function (n = 1, k, l) {
			r <- vector("numeric", length = n)
			for (i in 1:n) {
				t <- table(factor(sample(1:k, l, replace = TRUE), levels = 1:k),
					factor(sample(1:k, l, replace = TRUE), levels = 1:k))
				n <- sum(t)
				e1 <- sum(apply(t, 1, max), apply(t, 2, max))
				e2 <- max(rowSums(t)) + max(colSums(t))
				L <- 0.5 * (e1 - e2) / (n - 0.5 * e2)				
				r[ i ] <- L
			}
			return(r)
		}

		rl <- rl + sum(.randomLambda(n = nitr.lambda, k = k, l = nrow(x)))
	}
	close(pb)
	
	#	final modified lambda calculation
	l <- sum(l) / nitr
	rl <- rl / (nitr * nitr.lambda)
	ml <- (l - rl) / (1 - rl) # modified lamda
	
	r <- list(lambda = l, modified.lambda = ml, random.lambda = rl)
	return(r)
}


#	x <- VegsoupPartition(coenoflex(), k = 3)
#	plot(x)
#	stable(x)
#	i <- sapply(2:10, function (x) stable(VegsoupPartition(coenoflex(), k = x))$lambda)

#if (!isGeneric("stable"))) {
setGeneric("stable",
	function (x, nitr = 200, nitr.l = 10, ...)
		standardGeneric("stable")
)
#}

setMethod("stable",
	signature(x = "VegsoupPartition"),
	.stable
)
