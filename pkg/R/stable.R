#	adopted for vegsoup from script Classification Stability supplied for JUICE
#	http://www.sci.muni.cz/botany/juice/R/classification%20stability.r

.stable <- function (x, nitr = 200, nitr.l = 10, ...)	{
	
	k <- getK(x)
	
	pb <- txtProgressBar (min = 1, max = nitr, char = ".",
		width = 45, style = 3)
	
	# results vector to store raw lamda values
	r.l <- 0
	# results vector to store lamda
	l <- vector(mode = "numeric", length = nitr)
	
	for (i in 1:nitr) {
		setTxtProgressBar(pb, i)
		# subsets created by the without-replacement bootstrap
		sub <- vegsoup::sample(x, replace = TRUE)
		
		#	subset classification
		X <- VegsoupPartition(sub, k = k, method = x@partitioning.method, ...)
		#	common rows
		Y <- x[rownames(x) %in% rownames(X), ]
		
		#	raw lambda
		l[i] <- confusion(X, Y)$l
		
		#	random values for lambda
		#	see confusion.R
		.randomLambda <- function (n = 1) {
			r <- vector("numeric", length = n)
			for (i in 1:n) {
				t <- table(factor(sample(1:k, nrow(X), replace = TRUE), levels = 1:k),
					factor(sample(1:k, nrow(X), replace = TRUE), levels = 1:k))
				n <- sum(t)
				e1 <- sum(apply(t, 1, max), apply(t, 2, max))
				e2 <- max(rowSums(t)) + max(colSums(t))
				L <- 0.5 * (e1 - e2) / (n - 0.5 * e2)				
				r[ i ] <- L
			}
			return(r)
		}
		
		r.nitr.l <- .randomLambda(nitr.l)

		r.l <- r.l + sum(r.nitr.l)
	}
	close(pb)
	
	#	final modified lambda calculation
	l <- sum(l) / nitr
	r.l <- r.l / (nitr * nitr.l)
	l.m <- (l - r.l) / (1 - r.l) # modified lamda
	
	r <- list(lambda = l, modified.lambda = l.m)
	return(r)
}

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
