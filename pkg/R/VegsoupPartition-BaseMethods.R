#	subsetting method
#	to do: documentation
setMethod("[",
    signature(x = "VegsoupPartition",
    i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE) {
		#	x <- prt; i = Partitioning(prt) %in% c(1,10)
	    part <- Partitioning(x)
	    
	    if (missing(i)) i <- rep(TRUE, nrow(x))
	    if (missing(j)) j <- rep(TRUE, ncol(x))
	    
	    tmp <- as(x, "Vegsoup")
	    tmp <- tmp[i, j]#, ...]
        
        if (FALSE) {  # a little bit too verbose         	
			if (length(unique(part[names(part) %in% rownames(tmp)])) != getK(x)) {
				message(" Partitioning vector was subsetted!",
					" k was changed accordingly")
			}
		}
		
		#	develop class VegsoupPartition from class Vegsoup
		res <- new("VegsoupPartition", tmp)
	
		res@part <- part[names(part) %in% rownames(tmp)]
		if (!identical(names(res@part), rownames(tmp))) {
			stop("inconsistency when subsetting partitioning vector")
		}
		
		k <- length(unique(res@part))
		res@part[] <- as.integer(as.character(factor(res@part, labels = 1:k)))
		
		#	reassign remaining slots
		res@method = x@method
		res@k = k
		res@group = res@group[names(part) %in% rownames(tmp)]

	    return(res)
    }
)

#	Fisher Test
#	depreciated
#	use Fidelity(obj, "Fisher") instead

setGeneric("FisherTest",
	function (obj, ...)
		standardGeneric("FisherTest")
)
setMethod("FisherTest",
	signature(obj = "VegsoupPartition"),
	function (obj, alternative = "two.sided") {

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
	N_pi <- table(Partitioning(obj))	# number of plots in partition
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
#		res <- multipatt(as.logical(obj), Partitioning(obj),
#			...)
#		return(res)
#	}
#)

#	Indicator value minimizing intermediate occurrences
setGeneric("Isamic",
	function (obj)
		standardGeneric("Isamic")
)
setMethod("Isamic",
	signature(obj = "VegsoupPartition"),
	function (obj) { 	
	   	tmp <- constancy(obj) / 100
    	res <- apply(tmp, 1, function (x) {
    			2 * sum(abs(as.numeric(x)- 0.5)) / ncol(tmp)
    		})
    	return(res)
    }
)

#	Indicator species analysis by Murdoch preference function
#	to do: documentation
setGeneric("Murdoch",
	function (obj, ...)
		standardGeneric("Murdoch")
)
setMethod("Murdoch",
    signature(obj = "VegsoupPartition"),
    function (obj, minplt, type, ...) {
    	require(optpart)
    	if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))
    	if (missing(minplt))
    		minplt <- 1
		if (missing(type))
			part <- Partitioning(obj)
			
		if (any(table(part) == 1)) {
			stop("singleton present")
		}	
		ks <- getK(obj)
		res <- matrix(0, nrow = dim(obj)[2], ncol = ks)
		dimnames(res) <- list(colnames(obj), 1:ks)
		pval <- matrix(0, nrow = dim(obj)[2], ncol = ks)
		dimnames(pval) <- list(colnames(obj), 1:ks)
		res.ls <- vector("list", length = ks)
		names(res.ls) <- 1:ks
		for (i in 1:ks) {
			res.ls[[i]] <- optpart::murdoch(as.logical(obj),
				part == i, minplt = minplt)
			res[,i] <- res.ls[[i]]$murdoch
			pval[,i] <- round(res.ls[[i]]$pval, 3)
		}
    	return(c(res.ls, list(murdoch = res, pval = pval)))
    }
)

#	Silhouette Analysis
#	to do: documentation
#	dots argument does not work with cluster::silhouette
setGeneric("silhouette",
	function (x, ...)
		standardGeneric("silhouette")
)

setMethod("silhouette",
    signature(x = "VegsoupPartition"),
    function (x, ...) {
    	require(cluster)
		if (getK(x) == 1)
			stop("meaningless with k = ", getK(x))
    	
		res <- cluster::silhouette(Partitioning(x), dist = as.dist(obj, ...))
		return(res)    	
    }
)

#	dissimilarity diameters
if (!isGeneric("Disdiam")) {
setGeneric("Disdiam",
	function (obj, ...)
		standardGeneric("Disdiam")
)
}

setMethod("Disdiam",
    signature(obj = "VegsoupPartition"),
    function (obj, ...) {
    	require(optpart)
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))    	
		res <- optpart::disdiam(Partitioning(obj), as.dist(obj, ...))
		return(res)    	
    }
)