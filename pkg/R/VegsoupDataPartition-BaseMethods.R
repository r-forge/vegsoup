#	generating function
#	to do: implement formula interface for method, high priority
VegsoupDataPartition <- function (obj, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "external"), binary = TRUE, clustering, polish = FALSE, nitr = 999, seed = 1234, verbose = FALSE, ...) {

#	debug
#	obj = dta; binary = TRUE; k = 3;
#	method = "isopam"
#	dist = "bray"
#	nitr = 99; polish = TRUE
#	method = "external"
#	clustering = "syntaxon"
			
	if (!inherits(obj, "VegsoupData")) {
		stop("Need object of class VegsoupData")
	}
	if (missing(k) & missing(clustering) & !inherits(obj, "VegsoupDataOptimstride")) {
		k = 1
		warning(" argument k missing, set to ", k, call. = FALSE)
	}	
	if (missing(k) & inherits(obj, "VegsoupDataOptimstride")) {
		#	warning! no sensible results so far!
		k = summary(opt)$best.optimclass1
		#	k = as.numeric(strsplit(names(k[which.max(k)]), ".", fixed = TRUE)[[1]][2])
	}
	if (missing(binary)) {
		if (verbose) {
			cat("... Set to binary")
			binary = TRUE
		}	
	}	
	if (missing(k) && missing(clustering)) {
		stop("Need a value of k or optional clustering vetcor")
	}	
	if (missing(method) & missing(clustering)) {
		part.meth <- method <- "flexible"	
		if (verbose) {
			cat("... Set default option", part.meth)
		}	
	} else {
		METHODS <- c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "external")
		part.meth <- match.arg(method, METHODS)
	}
	if (!missing(clustering) | match.arg(method) == "external") {
		if (missing(clustering)) {
			warning(" selected method external but did not define clustering", call. = FALSE)
		}
		part.meth <- method <- "external"
		if (length(clustering) == 1) {		
			sel <- pmatch(clustering, names(Sites(obj))) # rename to: names(sites)
			if (!is.na(sel)) {			
				clustering = as.vector(Sites(obj)[, sel]) 			
			} else {
				stop("if length of clustering is 1",
				" the argument has to match",
				" a column name of Sites(obj)")
			}				
		}
		if (length(clustering) == nrow(obj)) {
			k <- length(unique(clustering))
			if (verbose) {
				cat("... Use supplied vector, number of partitons ",
					ifelse(is.integer(k), k, as.integer(k)))
			}	
		} else {
			stop("... length of clustering vector",
				" and nrow(obj) have to match",
				dim(obj), length(clustering))
		}
	}		
	#	species and distnce matrices
	if (binary) {
		X <- as.logical(obj)
	} else {
		X <- as.numeric(obj)
	}	
	if (part.meth != "external") {	
		Xd <- as.dist(obj)
	}

	#	print settings before run
	if (verbose) {
		cat("\n run with settings",
			"\n use binary data:", binary,
			"\n disimilarity measure:", vegdist(obj),
			"\n decostand method:", decostand(obj),
			"\n partitioning method:", part.meth, "\n")	
	}
#	set seed
	set.seed(seed)		
#	partitioning methods
switch(part.meth,
	   ward = {
	   	part <- agnes(Xd, method = "ward",
	   		...)	   
	 }, flexible = {
	   	alpha <- 0.625
	   	beta = 1 - 2 * alpha
	   	part <- agnes(Xd, method = "flexible",
	   		par.meth = c(alpha, alpha, beta, 0),
	   		...)
	}, pam = {
		if (verbose) cat("\nrun pam")
		part <- pam(Xd, k = k, diss = TRUE,
			...)
	}, isopam = {
		if (verbose) cat("\nrun isopam, ignoring k=", k)
		if (verbose) cat("\nplease supply c.fix to restict to a specific number of partitions\n")
		#	if (binary) tmp <- as.logical(obj) else tmp <- as.numeric(obj)
		part <- isopam(X, distance = vegdist(obj),
			...)
	}, optpart = {
		if (verbose) cat("\nrun optpart from random starts ...")
		if (verbose) cat("\nset to", k, "partitions\n")
		part <- .VegsoupDataPartitionOptpartBestopt(Xd, k, numitr = 100,
			...)
	}, kmeans = {
		if (verbose) cat("kmeans doesn't use distance matrices, ignore", vegdist(obj))
		part <- kmeans(X, centers = k,
			...)		
	}, wards = {
		part <- hclust(Xd, method = "ward",
			...)
	}, external = {
		part <- clustering
	}
)
if (inherits(part, "agnes") | inherits(part, "hclust")) {
	grp <- cutree(part, k)
	names(grp) <- rownames(obj)
}
if (inherits(part, "kmeans")) {
	grp <- part$cluster
}
if (inherits(part, "pam")) {
	grp <- part$clustering
}
if (inherits(part, "isopam")) {
	if (is.null (part$hier)) {
		if (verbose) cat("no hierarchy estimated by isopam")
	  	grp <- part$flat
	  	k  <- length(unique(part$flat))
	} else { 
		if (verbose) cat("retieve lowest hierachy level")
		grp <- part$flat[[ncol(part$hier)]]
		k <- length(unique(grp))
	}
}
if (inherits(part, "partana")) {
	grp <- part$clustering
	names(grp) <- rownames(obj)
}
if (is.vector(part)) { # method external
	grp <- as.numeric(factor(part))
	# warning: prone to error if clustering is not selected from sites!
	names(grp) <- rownames(obj)
	if (verbose) {
		print(data.frame(clustering = levels(factor(clustering)),
			assigned = as.numeric(factor(levels(factor(clustering)))) )
		)
		
	}
}
if (k != length(unique(grp)) && class(part) != "isopam") {
	warning(" did not converge for", k, "partitions",
		"\nset k to", length(unique(grp)), call. = FALSE)
}
	
#	if method return outgroup try to refine clustering
#	using optimal sil widths (polish == TRUE)
#	may comsume significant cpu time!

out.grp <- any(as.vector(table(grp)) == 1)

if (out.grp) {
	warning(" single member groups detected!", call. = FALSE)
}
#	fundamental change! 
if (out.grp && polish) { # was ||
	if (verbose) cat("\n... try to resolve using function optsil")
	grp.opt <- optsil(grp, Xd, k^2)$clustering
	names(grp.opt) <- rownames(obj)
	if (any(as.vector(table(grp.opt)) == 1)) {
		warning(" did not succeed in reallocation", call. = FALSE)
	} else {
		method <- c(method, "optsil")	
		grp <- grp.opt
		cat("\n successfully 'polished' clustering")
		if (k !=  length(unique(grp))) {
			cat("\n reset intial k =", k,
				"to k =", length(unique(grp)), "\n")
		}
	}	
}

#	develop class VegsoupDataPartition from class VegsoupData
res <- new("VegsoupDataPartition", obj)
#	assign class slots
res@part <- grp
res@method <- part.meth	
res@k <- length(unique(grp))
res@binary = binary

return(res)
}

#	subsetting method
#	to do: documentation
setMethod("[",
    signature(x = "VegsoupDataPartition",
    i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE) {
		#	x <- prt; i = Partitioning(x) == 2
	    part <- Partitioning(x)
	    
	    if (missing(i)) i <- rep(TRUE, nrow(x))
	    if (missing(j)) j <- rep(TRUE, ncol(x))
	    
	    tmp <- as(x, "VegsoupData")
	    tmp <- tmp[i, j, ...]
        
#		if (length(unique(part[names(part) %in% rownames(tmp)])) != getK(x)) {
#			warning(" Partitioning vector was subsetted!",
#				" k was changed accordingly", call. = FALSE)
#		}

		#	develop class VegsoupDataPartition from class VegsoupData
		res <- new("VegsoupDataPartition", tmp)
		res@part = part[names(part) %in% rownames(tmp)]
		res@method = x@method
		# was res@dist = x@dist # now slot of class VegsoupData
		res@k = length(unique(part[names(part) %in% rownames(tmp)]))
		res@group = res@group[names(part) %in% rownames(tmp)]

	    return(res)
    }
)

.plotVegsoupPartition <- function (x, ...) {
	#	x = prt
#	op <- par()
#	on.exit(par(op))
	
	if (!inherits(x, "VegsoupDataPartition")) stop
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
	signature(x = "VegsoupDataPartition", y = "missing"),
	.plotVegsoupPartition
)

#	getter method
#	running partition vector
if(!isGeneric("Partitioning")) {
setGeneric("Partitioning",
	function (obj)
		standardGeneric("Partitioning")
)
}

#	retrieve or set slot part
setMethod("Partitioning",
	signature(obj = "VegsoupDataPartition"),
	function (obj) obj@part
)

#	replace slot part
if(!isGeneric("Partitioning<-")) {
setGeneric("Partitioning<-",
	function (obj, value, ...)
		standardGeneric("Partitioning<-")
)
}
setReplaceMethod("Partitioning",
	signature(obj = "VegsoupDataPartition", value = "numeric"),
	function (obj, value) {
		if (length(value) != length(Partitioning(obj))) {
			stop("\n replacement does not match in length")
		}
		if (is.null(names(value))) {		
			names(value) <- rownames(obj)
		} else {
			if (length(intersect(names(value), rownames(obj))) != nrow(obj)) {
				stop("\n if value has names, these have to match rownames(obj)")
			} else {
				value <- value[match(rownames(obj), names(value))]
			}
		}
		obj@part <- value
		obj@k <- length(unique(value))
		
		return(obj)		
	}
)

#	number of partitions/clusters
#	to do: documentation
setGeneric("getK",
	function (obj)
		standardGeneric("getK")
)
setMethod("getK",
	signature(obj = "VegsoupDataPartition"),
	function (obj) obj@k
)

#	list of species occurence in clusters
#	to do: documentation
setGeneric("Spread",
	function (obj)
		standardGeneric("Spread")
)
#	to do: speed up?
setMethod("Spread",
	signature(obj = "VegsoupDataPartition"),
	function (obj) {
		part  <- Partitioning(obj)
		com <- as.logical(obj)
		if (length(unique(part)) == 1)
			warning(" only a single partition present", call. = FALSE)
		res <- apply(com, 2, function (y) {
			sapply(rownames(com[y > 0,]), # y
				function (z) part[which(names(part) == z)],
				USE.NAMES = FALSE)
			})
	return(res)
	}
)

#	summary funection
#	to do: documentation
setMethod("summary",
    signature(object = "VegsoupDataPartition"),
	function (object, choice = c("all", "species", "sites", "partition"), ...) {
		if (missing(choice)) {
			choice <- "all"
		}
		choices <- c("all", "species", "sites", "partition")
		choice <- choices[pmatch(choice, choices)]
		if (is.na(choice)) {
			choice <- "all"
		}
		switch(choice, "all" = {
			summary(as(object, "VegsoupData"), choice = "all")
			cat("\ntable of partition contingencies")
			print(table(Partitioning(object)))
		}, "species" = {
			summary(as(object, "VegsoupData"), choice = "species")
		}, "sites" = {
			summary(as(object, "VegsoupData"), choice = "sites")
		}, "partition" = {
			cat("\ntable of partition contingencies")
			print(table(Partitioning(object)))	
		})	
	}
)

#	contingency table

#	to do: documentation
setGeneric("Contingency",
	function (obj)
		standardGeneric("Contingency")
)

setMethod("Contingency",
	signature(obj = "VegsoupDataPartition"),
	function (obj) {
		res <- t(aggregate(as.logical(obj),
			by = list(Partitioning(obj)), FUN = sum))[-1, ]
		colnames(res) <- unique(Partitioning(obj))
		rownames(res) <- colnames(obj)
		return(res)
	}
)

#	frequency (constancy) table	
#	to do: documentation
setGeneric("Constancy",
	function (obj, ...)
		standardGeneric("Constancy")
)
setMethod("Constancy",
	signature(obj = "VegsoupDataPartition"),
	function (obj, percentage = TRUE, ...) {
		tmp <- t(as.matrix(table(Partitioning(obj))))
		res <- Contingency(obj) / tmp[rep(1, nrow(Contingency(obj))),]
		if (percentage) {
			res <- round (res * 100, 0)
		}		
		return(res)
	}
)

#	shared species
setGeneric("Shared",
	function (obj, ...)
		standardGeneric("Shared")
)

setMethod("Shared",
	signature(obj = "VegsoupDataPartition"),
	function (obj, ...) {
		X <- Constancy(obj) > 0
		mode(X) <- "numeric"
		
		res <- designdist(t(X), method = "J/(A+B)*100", terms = "binary")

	return(res)
	}
)


#	summary statistics
#	Tukey Five-Number Summary
setGeneric("Fivenum",
	function (obj, ...)
		standardGeneric("Fivenum")
)

setMethod("Fivenum",
	signature(obj = "VegsoupDataPartition"),
	function (obj, na.rm = TRUE, recode = FALSE) {
		tmp <- as.numeric(obj)
		if (!na.rm) tmp[tmp == 0] <- NA
		tmp <- aggregate(tmp,
			by = list(Partitioning(obj)),
			FUN = function (x) fivenum(x, na.rm = TRUE), simplify = FALSE)
		part <- tmp[, 1]
		tmp <- tmp[, -1]
		res <- array(0, dim = c(dim(tmp)[2], dim(tmp)[1], 5),
			dimnames = list(names(tmp), part,
			c("min", "lower", "median", "upper", "max")))
		for (i in 1:5) {
			for (j in 1:nrow(res)) {
				#	j = 1; i = 1
				res[j, , i] <- sapply(tmp[, j], function (x) x[i])
			}
		}
		if (recode) {
			for (i in 1:dim(res)[3]) {
				tmp <- res[, , i]
				mode(tmp) <- "numeric"
				vals <- as.character(cut(tmp,
					breaks = c(0, coverscale(obj)@lims),
					labels = coverscale(obj)@codes))
				tmp.i <- tmp
				mode(tmp.i) <- "character"
				tmp.i[] <- vals
				res[, , i] <- tmp.i
			}			
		res[is.na(res)] <- "."
		}
		return(invisible(res))	
	}
)

#	Fisher Test
#	depreciated
#	use Fidelity(obj, "Fisher") instead
#	to do: documentation

setGeneric("FisherTest",
	function (obj, ...)
		standardGeneric("FisherTest")
)
setMethod("FisherTest",
	signature(obj = "VegsoupDataPartition"),
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
	signature(obj = "VegsoupDataPartition"),
	function (obj) {
	
	cnti <- Contingency(obj)
	cnst <- Constancy(obj)
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

#	Dufrene & Legendre's indicator value
#	to do: documentation
#	See also \code{\link{Fidelity}}, \code{\link{Phi}} and \code{\link{FisherTest}}
setGeneric("Indval",
	function (obj, ...)
		standardGeneric("Indval")
) 
setMethod("Indval",
	signature(obj = "VegsoupDataPartition"),
	function (obj, ...) {
		res <- indval(as.logical(obj), Partitioning(obj), ...)$indval
		return(res)
	}
)

#	indicator species analysis by combining groups of sites
#setGeneric("Multipatt",
#	function (obj, ...)
#		standardGeneric("Multipatt")
#) 
#setMethod("Multipatt",
#	signature(obj = "VegsoupDataPartition"),
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
	signature(obj = "VegsoupDataPartition"),
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
    signature(obj = "VegsoupDataPartition"),
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

#	optimise partitioning using Dave Roberts optsil procedure
setGeneric("Optsil",
	function (obj, ...)
		standardGeneric("Optsil")
)
setMethod("Optsil",
    signature(obj = "VegsoupDataPartition"),
    function (obj, maxitr = 100, verbose = FALSE, ...) {
		require(optpart)
		if (getK(obj) == 1) stop("meaningless with k = ", getK(obj))
    	
    	nam <- names(obj@part) # save names    	
    	cl <- match.call()
    	    	
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}   	  	
    		
		cpu.time <- system.time({
			tmp <- optpart::optsil(
					x = Partitioning(obj), dist = as.dist(obj, ...),
					maxitr = maxitr)
			obj@part <- as.integer(tmp$clustering)
			numitr <- tmp$numitr			
		})

		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", numitr)	
		}	
	
		names(obj@part) <- nam
		return(obj)
	}
)

#	optimise partitioning using Dave Roberts optindval procedure
setGeneric("Optindval",
	function (obj, ...)
		standardGeneric("Optindval")
)
setMethod("Optindval",
    signature(obj = "VegsoupDataPartition"),
    function (obj, maxitr = 100, minsiz = 5, verbose = FALSE, ...) {
		require(optpart)
		
    	if (getK(obj) == 1) stop("meaningless with k = ", getK(obj))

    	nam <- names(obj@part) # save names		
		cl <- match.call()
		
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}

		cpu.time <- system.time({
			tmp <- optpart::optindval(
					as.matrix(obj, ...), Partitioning(obj),
					maxitr = maxitr,
					minsiz = minsiz)
			obj@part <- as.integer(tmp$clustering)		
			numitr <- tmp$numitr		
		})
		if (verbose) {
			cat("\n time to optimise species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n number of iterations performed:", numitr)	
		}					
		names(obj@part) <- nam
		return(obj)
    }
)

#	Partition Analysis
#	to do: check dist slot
#	to do: implement ?testpart()
setGeneric("Partana",
	function (obj, ...)
		standardGeneric("Partana")
)

setMethod("Partana",
    signature(obj = "VegsoupDataPartition"),
    function (obj, verbose = FALSE, ...) {
		require(optpart)
		if (getK(obj) == 1)	stop("meaningless with k = ", getK(obj))
    	Xd <- as.dist(obj, ...)   	

		cpu.time <- system.time({		
			res <- optpart::partana(c = Partitioning(obj), dist = Xd)
		})
		if (verbose) {
			cat("\n time to permute species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n within-cluster to among-cluster similarity ratio:",
				round(res$ratio, 1))		
		}					
		return(invisible(res))    	
    }
)

#	table deviance
setGeneric("Tabdev",
	function (obj, ...)
		standardGeneric("Tabdev")
)

setMethod("Tabdev",
	signature(obj = "VegsoupDataPartition"),
	function (obj, numitr = 99, verbose = FALSE, ...) {
		require(optpart)

		if (getK(obj) == 1) stop("meaningless with k = ", getK(obj))

		cpu.time <- system.time({
			res <- optpart::tabdev(as.matrix(obj, ...),
				Partitioning(obj), nitr = numitr, ...)
		})
		if (verbose) {
			cat("\n time to permute species matrix of", ncell(obj), "cells",
				"and", getK(obj), "partitions:",
				cpu.time[3], "sec")
			cat("\n total deviance:", res$totdev)	
			cat("\n number of iterations performed:", numitr)	
		}			
		return(invisible(res$spcdev))
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
    signature(obj = "VegsoupDataPartition"),
    function (obj, method , ...) {
    	require(cluster)
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))
    	
		res <- cluster::silhouette(Partitioning(obj), dist = obj)
		return(res)    	
    }
)

###	broken!
#	typal samples in a partition
setGeneric("Typal",
	function (obj, ...)
		standardGeneric("Typal")
)
setMethod("Typal",
    signature(obj = "VegsoupDataPartition"),
    function (obj, k = 2, ...) {
    	
"TYPAL" <- function (clustering, dist, k = 1) 
{
	require(optpart)
    clustering <- as.numeric(clustering)
    print(class(dist))
    if (class(dist) != "dist") {
        stop("typal is not defined for classes other than dist")
    }
    classes <- 1:length(table(clustering))
    part <- optpart::partana(clustering, dist)
    sil <- cluster::silhouette(clustering, dist)
    part.out <- matrix(NA, nrow = max(classes), ncol = k)
    for (i in classes) {
        tmp <- clustering == i
        names <- part$names[tmp]
        vals <- part$ptc[tmp, i]
        part.out[i, ] <- names[rev(order(vals))][1:k]
    }
    part.out <- data.frame(part.out)
    names(part.out) <- as.character(seq(1:k))
    sil.out <- matrix(NA, nrow = max(classes), ncol = k)
    for (i in classes) {
        tmp <- clustering == i
        names <- attr(dist, "Labels")[tmp]
        vals <- sil[tmp, 3]
        sil.out[i, ] <- names[rev(order(vals))][1:k]
    }
    sil.out <- data.frame(sil.out)
    names(sil.out) <- as.character(seq(1:k))
    out <- list(partana = part.out, silhouette = sil.out)
    out
}
    	require(optpart)    	
    	cl <- match.call()    	
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}
		if (getK(obj) == 1) {
			warning(" results are meaningless with k = ",
				getK(obj), call. = FALSE)
			return(NULL)
		}
		else {
			#d <- as.dist(obj, ...)
			p <- Partitioning(obj)
			d <- vegan::vegdist(as.matrix(obj))
			print(class(d))
   			res <- TYPAL(clustering = p, dist = d, k = k)
	   		return(res)
   		}
    }
)

#	'head' like print function based on identification of
#	typal samples in a partition

#	head is a primitive function
setMethod("head",
    signature(x = "VegsoupDataPartition"),
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
#	to do: documentation		
setGeneric("Disdiam",
	function (obj, ...)
		standardGeneric("Disdiam")
)

setMethod("Disdiam",
    signature(obj = "VegsoupDataPartition"),
    function (obj, method, ...) {
    	require(optpart)
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))    	
		res <- optpart::disdiam(Partitioning(obj), as.dist(obj))
		return(res)    	
    }
)

#	tabulate partition vector to matrix
setGeneric("PartitioningMatrix",
	function (obj)
		standardGeneric("PartitioningMatrix")
)

setMethod("PartitioningMatrix",
    signature(obj = "VegsoupDataPartition"),
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
	signature(obj = "VegsoupDataPartition"),
	.PartitioningCombinations
)

#	List occurences of species in partitions
#	to do: documentation
setGeneric("Spread",
	function (object)
		standardGeneric("Spread")
)

setMethod("Spread",
    signature(obj = "VegsoupDataPartition"),
	function (object) {
		
	part  <- Partitioning(object)
	X <- as.logical(object)

	res <- apply(X, 2, function (y) {
		if (getK(object) > 1) {
			sapply(rownames(X[y > 0,]),
				function (z) part[which(names(part) == z)],
					USE.NAMES = FALSE)
		} else {
			warning(" a single partition is not meaningful", call. = FALSE)
		}
	}
	)
	return(res)
	}
)