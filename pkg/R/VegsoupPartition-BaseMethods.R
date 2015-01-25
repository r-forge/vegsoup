#	Fisher Test
#	depreciated, use fidelity(obj, "Fisher") instead

setGeneric("FisherTest",
	function (obj, ...)
		standardGeneric("FisherTest")
)
setMethod("FisherTest",
	signature(obj = "VegsoupPartition"),
	function (obj, alternative = "greater") {

#	apapted from isotab.R (package 'isopam')
#	which borrowed by itself from fisher.test

	alternative <-  match.arg(as.character(alternative), c("greater","less","two.sided"))
#	P-value of Fisher test
	FisherPval <- function (x) {
		p <- NULL
		m <- sum(x[, 1])
		n <- sum(x[, 2])
		k <- sum(x[1, ])
		x <- x[1, 1]
		lo <- max(0, k - n)
		hi <- min(k, m)
		support <- lo:hi
		logdc <- dhyper(support, m, n, k, log = TRUE)

		dnhyper <- function (ncp) {
			d <- logdc + log(ncp) * support
			d <- exp(d - max(d))
			d / sum(d)
		}
		
		pnhyper <- function (q, ncp = 1, upper.tail = FALSE) {
			if (ncp == 1) {
				if (upper.tail) { 
					return(phyper(x - 1, m, n, k, lower.tail = FALSE))
				} else {
					return(phyper(x, m, n, k))
				}
			}
			if (ncp == 0) {
				if (upper.tail) {
					return(as.numeric(q <= lo))
				} else {
					return(as.numeric(q >= lo))
				}
			}
			if (ncp == Inf) {
				if (upper.tail) {
					return(as.numeric(q <= hi))
				} else {
					return(as.numeric(q >= hi))
				}
			}
		
			d <- dnhyper(ncp)
		
			if (upper.tail) {
				sum(d[support >= q])
			} else {
				sum(d[support <= q])
			}
		}

		res <- switch(alternative,
			less = pnhyper(x, 1),
			greater = pnhyper(x, 1, upper.tail = TRUE),
			two.sided = {
					relErr <- 1 + 10^(-7)
					d <- dnhyper(1)
					sum(d[d <= d[x - lo + 1] * relErr])
			})
	return(res)
}	

#	2 x 2 contingency table of observed frequencies
#	natation follows Bruelheide (1995, 2000)
#	cited in Chytry et al 2002:80

	ObservedFreqencyTable <- function (N, N_p, n, n_p) {
		res <- matrix(c(
			n_p,
			N_p - n_p,
			n - n_p,
			N - N_p - n + n_p), 2, 2)
		res[is.nan(res)] <- 0
		return(res)	
	}

	N <- nrow(obj)						# number of plots
	n_i <- colSums(as.logical(obj))		# species frequencies
	N_pi <- table(partitioning(obj))	# number of plots in partition
	n_pi <- contingency(obj)			# number of occurences in partition

	res <- n_pi

	for (i in 1:ncol(obj)) { # loop over species
		n <- n_i[i]						# n	
		for (j in 1:length(N_pi)) {		# loop over partitions
			N_p <- N_pi[j]
			n_p <- n_pi[i,j]
			res[i, j] <- FisherPval(ObservedFreqencyTable(N, N_p, n, n_p))
		}
	}
	
	return(res)
}
)

#	indicator species analysis by combining groups of sites
#setGeneric("Multipatt",
#	function (obj, ...)
#		standardGeneric("Multipatt")
#) 
#setMethod("Multipatt",
#	signature(obj = "VegsoupPartition"),
#	function (obj, ...) {
#		if (getK(obj) == 1) {
#			stop("meaningless with k = ", getK(obj))
#		}	
#		res <- multipatt(as.logical(obj), partitioning(obj),
#			...)
#		return(res)
#	}
#)