#	standardised Phi statistic
#	depreciated, maybe usefull to speed up things where Fidelity() is too slow
#	preferred method is Fidelity(obj, func = "r.g")
#	to do: documentation
#	See also \code{\link{Fidelity}}, \code{\link{Indval}} and \code{\link{FisherTest}}

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
	siz <- table(Partitioning(obj))
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
			res.1 <- nv <- (N * oc - total * cs) 
			res.2 <- sqrt(total * cs * (N - total) * (N - cs))            
			nv <- res.1 / res.2
			res[i,j] <- nv
		}
	}
	
	res[is.na (res)] <- 0

	return(res)
}
)
