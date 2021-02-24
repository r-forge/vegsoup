#	Tichý, L. and Chytrý, M. and Šmarda, P. (2011) Evaluating the stability of
#	the classification of community data. Ecography 34:807-813
#	adopted for vegsoup from script Classification Stability supplied for JUICE
#	http://www.sci.muni.cz/botany/juice/R/classification%20stability.r

.stable <- function (x, nitr = 200, nitr.lambda = 10, seed = 1234, ...)	{
	#	setting seed
	old.seed <- .Random.seed
	on.exit( { .Random.seed <<- old.seed } )
	set.seed(seed)
	
	k <- getK(x)

	#	classification of original data set
	o <- partitioning(x)
	
	pb <- txtProgressBar (min = 1, max = nitr, char = ".",
		width = 45, style = 3)
	
	# results vector to store lamda values
	l <- vector(mode = "numeric", length = nitr)

	# results vector to store random lamda values
	rl <- 0
		
	for (i in 1:nitr) {
		setTxtProgressBar(pb, i)

		#	subsets created by the without-replacement bootstrap
		#	xi <- vegsoup::sample(x, replace = TRUE) # default of size = nrow(x) # aquivalnet
		xi <- x[ unique(sort(sample(c(1:nrow(x)), replace = TRUE))), ]
		#	classification of bootstrap sample 
		s <- VegsoupPartition(xi, k = k, method = x@partitioning.method, ...)
		s <- partitioning(s)
		#	only sites present in the sample are considered
		os <- o[ names(o) %in% names(s) ]	
		#	calculate lamda (cp. confusion.R)
		l[ i ] <- .lambda(table(s, os))		
		#	random values for lambda for size of subset
		for (j in 1:nitr.lambda) {
   			rl <- rl + .lambda(table(
				factor(sample(1:k, length(s), replace = TRUE), levels = 1:k),
				factor(sample(1:k, length(s), replace = TRUE), levels = 1:k)))
		}	
	}
	close(pb)
	
	#	final modified lambda calculation
	l <- sum(l) / nitr
	rl <- rl / (nitr * nitr.lambda) # mean of random lambda values
	ml <- (l - rl) / (1 - rl) # modified lamda see Tichy et al. eq. 2
	
	r <- list(lambda = round(l, 3),
		modified.lambda = round(ml, 3),
		random.lambda = round(rl, 3))
	return(r)
}

if (!isGeneric("stable")) {
setGeneric("stable",
	function (x, nitr = 200, nitr.l = 10, ...)
		standardGeneric("stable")
) }

setMethod("stable",
	signature(x = "VegsoupPartition"),
	.stable
)
