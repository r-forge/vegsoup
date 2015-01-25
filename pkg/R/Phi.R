#	standardised Phi statistic
#	depreciated, maybe usefull to speed up things where fidelity() is too slow
#	preferred method is fidelity(obj, func = "r.g")

setGeneric("Phi",
	function (obj)
		standardGeneric("Phi")
)
setMethod("Phi",
	signature(obj = "VegsoupPartition"),
	function (obj) {
	
	cnti <- contingency(obj)
	cnst <- constancy(obj)
	nc <- ncol(cnst)
	N <- nrow(obj)
	SP <- ncol(obj)
	siz <- table(partitioning(obj))
	S <- 1 / nc	# constant S (Tichy et al 2006)
	cs <- S * N	# new cluster sizes
	res <- cnst

	for (i in 1:SP) {	# loop over species
		for (j in 1:ncol(cnst)) {	# loop over partitions
			insd <- cnti[i, j]					# original n in cluster j
			outs <- sum(cnti[i,-j])				# original n outside cluster j
			oc <- cs * (insd / siz[j])				# new n in cluster j
			on <- (N - cs) * (outs / (N - siz[j]))	# new n outside cluster j
			total <- oc + on						# new total value
			r1 <- nv <- (N * oc - total * cs) 
			r2 <- sqrt(total * cs * (N - total) * (N - cs))
			nv <- r1 / r2
			res[i,j] <- nv
		}
	}
	
	res[is.na (res)] <- 0

	return(res)
}
)
