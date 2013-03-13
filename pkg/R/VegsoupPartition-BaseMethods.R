#	subsetting method
#	to do: documentation
setMethod("[",
    signature(x = "VegsoupPartition",
    i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE) {
		#	x <- prt; i = Partitioning(x) == 2
	    part <- Partitioning(x)
	    
	    if (missing(i)) i <- rep(TRUE, nrow(x))
	    if (missing(j)) j <- rep(TRUE, ncol(x))
	    
	    tmp <- as(x, "Vegsoup")
	    tmp <- tmp[i, j, ...]
        
#		if (length(unique(part[names(part) %in% rownames(tmp)])) != getK(x)) {
#			warning(" Partitioning vector was subsetted!",
#				" k was changed accordingly", call. = FALSE)
#		}

		#	develop class VegsoupPartition from class Vegsoup
		res <- new("VegsoupPartition", tmp)
		res@part = part[names(part) %in% rownames(tmp)]
		res@method = x@method
		# was res@dist = x@dist # now slot of class Vegsoup
		res@k = length(unique(part[names(part) %in% rownames(tmp)]))
		res@group = res@group[names(part) %in% rownames(tmp)]

	    return(res)
    }
)

.plotVegsoupPartition <- function (x, ...) {
	#	x = prt
#	op <- par()
#	on.exit(par(op))
	
	if (!inherits(x, "VegsoupPartition")) stop
	cat("\nLet me calculate capscale first ...")
	cat("\nuse distance:", x@dist)

	#	capscale has difficulties when using community matrix in the formula
#	tmp <- 
	#cat(class(tmp))
	ord <- capscale(as.logical(x) ~ 1, data = Sites(x))
	#	number of axes shown in plot, default to frist 3
	axs <- matrix(c(1,2,1,3,2,3), 3,2, byrow = TRUE)
	
	#	ordisplom like display
	axs <- matrix(c(
		1,3,  2,3,  3,NA,
		1,2,  2,NA,	3,2,
		1,NA, 2,1,	3,1),
		9, 2, byrow = TRUE)	

	if (missing(y)) {
		cat("\nuse slot partition")
		grp <- as.factor(Partitioning(x))
	}
	if (!missing(y) && missing(ind)) {
		stop("please supply sites column name or index!")
	}
	if (!missing(y) && !missing(ind)) {
		cat("\nnot implemented yet")
	#	grp <- get.sites.variable(y, ind)	
	}

	scs <- scores(ord, display = "sites")

	if (!identical(dim(scs)[1], length(grp))) {
		cat("\nremoved some sites!")
		grp <- grp[match(rownames(scs), names(grp)),1]
	}

	par(mfrow = c(3,3), mar = rep(0,4))
				scs <- scores(ord,
					choices = unique(c(axs))[!is.na(unique(c(axs)))])
	apply(axs, 1, function (x) {
		if (!any(is.na(x))) {
			# x  <- axs[1,]
			lims <- range(scs$sites)
			fig <- ordiplot(ord, choices = c(x),
				type = "n", axes = FALSE,
				xlim = lims, ylim = lims)
			text(x = 0, y = par("usr")[3] - c(par("usr")[3] * 0.04),
				colnames(scs$sites)[x[1]])
			text(x = par("usr")[1] - c(par("usr")[1] * 0.04), y = 0,
				colnames(scs$sites)[x[2]], srt = 90)
			cents <- try(
				ordiellipse(fig, groups = grp,
					choices = c(x), conf = .95,
					draw = "polygon", lty = 0,
					col = rgb(0,0,0,.2)),
				silent = TRUE)
			if (class(cents) != "try-error") {
				labs <- sapply(cents, function (x) {
				c(x = x$center[1], y = x$center[2])
				})
				text(x = labs[1,], y = labs[2,],
					labels = dimnames(labs)[[2]],
					font = 2, cex = 2)
			}	
			points(fig, "sites")
	
			ordispider(fig, groups = grp, choices = c(x),
				lwd = c(1/.75)* .25 )
		} else {
			frame()
			plot.window(c(-1,1), c(-1,1))
			text(0,0, labels = colnames(scs$sites)[x[1]], cex = 2)
		}
	})
	return(invisible(ord))
}

#	plot method
setMethod("plot",
	signature(x = "VegsoupPartition", y = "missing"),
	.plotVegsoupPartition
)

#	number of partitions/clusters
#	to do: documentation
setGeneric("getK",
	function (obj)
		standardGeneric("getK")
)
setMethod("getK",
	signature(obj = "VegsoupPartition"),
	function (obj) obj@k
)

#	summary funection
#	to do: documentation
setMethod("summary",
    signature(object = "VegsoupPartition"),
	function (object, choice = c("all", "species", "sites", "partition"), ...) {
		if (missing(choice)) {
			choice <- "all"
		}
		choices <- c("all", "species", "sites", "partition")
		choice <- choices[pmatch(choice, choices)]
		if (is.na(choice)) {
			stop("invalid choice")
		}       	
    	if (choice == -1) 
        	stop("ambiguous choice")
        	
		switch(choice, "all" = {
			summary(as(object, "Vegsoup"), choice = "all")
			cat("\ntable of partition contingencies")
			print(table(Partitioning(object)))
		}, "species" = {
			summary(as(object, "Vegsoup"), choice = "species")
		}, "sites" = {
			summary(as(object, "Vegsoup"), choice = "sites")
		}, "partition" = {
			cat("\ntable of partition contingencies")
			print(table(Partitioning(object)))	
		})	
	}
)

#	contingency table

#	Fisher Test
#	depreciated
#	use Fidelity(obj, "Fisher") instead
#	to do: documentation

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
	n_pi <- Contingency(obj)			# number of occurences in partition

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
	   	tmp <- Constancy(obj) / 100
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
setGeneric("Silhouette",
	function (obj, ...)
		standardGeneric("Silhouette")
)

setMethod("Silhouette",
    signature(obj = "VegsoupPartition"),
    function (obj, method , ...) {
    	require(cluster)
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))
    	
		res <- cluster::silhouette(Partitioning(obj), dist = obj)
		return(res)    	
    }
)

#	'head' like print function based on identification of
#	typal samples in a partition

#	head is a primitive function
setMethod("head",
    signature(x = "VegsoupPartition"),
    function (x, n = 6L, choice = "species", ...) {
    	if (missing(choice))
	    	choice <- "species"
	    if (n != 6L) {
	    	sel <- match(c(as.matrix(typal(x, ...)$silhouette)),
	    		rownames(x))
	    	if (choice == "species")
    			res <- as.character(x)[sel,]
	    	if (choice == "sites")
    			res <- Sites(x)[sel,]
    	} else {
	    	if (choice == "species")
    			res <- head(as.character(x), ...)
	    	if (choice == "sites")
    			res <- head(as.character(x), ...)
    	}	
    	return(res)
    }    	    
)

#	dissimilarity diameters
#if (!isGeneric("Disdiam")) {
setGeneric("Disdiam",
	function (obj, method, ...)
		standardGeneric("Disdiam")
)
#}
setMethod("Disdiam",
    signature(obj = "VegsoupPartition"),
    function (obj, method, ...) {
    	require(optpart)
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))    	
		res <- optpart::disdiam(Partitioning(obj), as.dist(obj, ...))
		return(res)    	
    }
)

#	tabulate partition vector to matrix
#if (!isGeneric("PartitioningMatrix")) {
setGeneric("PartitioningMatrix",
	function (obj)
		standardGeneric("PartitioningMatrix")
)
#}
setMethod("PartitioningMatrix",
    signature(obj = "VegsoupPartition"),
	function (obj) {
		res <- t(sapply(Partitioning(obj),
			function (x) {
				as.numeric(x == levels(factor(Partitioning(obj))))
			} ))
		dimnames(res)[2] <- list(levels(factor(Partitioning(obj))))
    return(res)                                                                                                                              
	}
)

# Matrix of possible cluster (x) combinations
.PartitioningCombinations <- function (obj, collapse) {
	
if (missing(collapse)) {
	collapse = "+"
}

cluster <- levels(as.factor(Partitioning(obj)))

cl.comb <- function (x) {
	k <- length(x) # k <- getK(prt)
	ep <- diag(1, k, k)
	names.ep <- x
    for (j in 2:k) {
    	#cat(j)
    	nco <- choose(k,j)
    	co <- combn(k,j)
    	epn <- matrix(0, ncol = nco, nrow = k)
		for (i in 1:ncol(co)) {
		#cat(i)
		epn[co[,i],i] <- 1
		names.ep <- c(names.ep, paste(x[co[,i]], collapse = collapse))
		}
	ep <- cbind(ep,epn)
	}
	colnames(ep) <- names.ep
	return(ep)
}
res <- cl.comb(cluster)

}


#	Indicator value minimizing intermediate occurrences
setGeneric("PartitioningCombinations",
	function (obj, collapse)
		standardGeneric("PartitioningCombinations")
)
setMethod("PartitioningCombinations",
	signature(obj = "VegsoupPartition"),
	.PartitioningCombinations
)