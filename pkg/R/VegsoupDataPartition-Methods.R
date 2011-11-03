#	generating function
VegsoupDataPartition <- function (x, binary, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "external"), dist = "bray", clustering, polish = FALSE, nitr = 999, verbose = FALSE, ...) {

#	x	object of class VegsoupData
#	binary	use presence/absence only
#	number of partitions
#	method	clustering method
#	dist	a distance measure known by vegdist()
#	clustering	external vector of cluster partitons
#	polish	apply optsil clustering on possible outgroups
#	tabdev	return object of class "tabdev"
#	nitr	number of iterations in calculating table deviance
	
debug = FALSE

if (debug) {
	x = dta; binary = TRUE; k = 3;
	method = "isopam"
	dist = "bray"
	nitr = 99; polish = TRUE
	method = "external"
	clustering = Sites(dta)$association

} else {
	if (!inherits(x, "VegsoupData"))
		stop("Need object of class VegsoupData")
	if (missing(k) & missing(clustering)) {
		k <- 1
		warning("argument k missing, set to ", k)
	}	
	if (missing(binary) && method != "external") 
		if (verbose) cat("... Set to binary")
		binary = TRUE
	if (missing(k) && missing(clustering))
		stop("Need a value of k or optional clustering vetcor")
	if (missing(method)) {
		part.meth <- "flexible"	
		if (verbose) cat("... Set default option", part.meth)

	} else {			
		part.meth <- match.arg(method)
	}
	if (!missing(clustering) & method == "external") {
		if (length(clustering) == nrow(x)) {
			k <- length(unique(clustering))
			if (verbose) cat("... Use supplied vector, number of partitons ",
				ifelse(is.integer(k), k, as.integer(k)))
		} else {
			stop("... Length of clustering vector and matrix must match",
				dim(x), length(clustering))
		}
	}
	if (!binary) {
		dis <- vegdist(wisconsin(as.numeric(x)), dist)
	} else {
		dis <- vegdist(wisconsin(as.binary(x)), dist)
	}	
}

#	partitioning methods
switch(part.meth,
	   ward = {
	   	part <- agnes(dis, method = "ward",
	   		...)	   
	 }, flexible = {
	   	alpha <- 0.625
	   	beta = 1 - 2 * alpha
	   	part <- agnes(dis, method = "flexible",
	   		par.meth = c(alpha, alpha, beta, 0),
	   		...)
	}, pam = {
		if (verbose) cat("\nrun pam")
		part <- pam(dis, k = k, diss = TRUE,
			...)
	}, isopam = {
		if (verbose) cat("\nrun isopam, ignoring k=", k)
		if (verbose) cat("\nplease supply c.fix to restict to a specific number of partitions\n")
		if (binary) tmp <- as.binary(x) else tmp <- as.numeric(x)
		part <- isopam(tmp, distance = dist,
			...)
	}, optpart = {
		if (verbose) cat("\nrun optpart from random starts ...")
		if (verbose) cat("\nset to", k, "partitions\n")
		part <- .VegsoupDataPartitionOptpartBestopt(dis, k, numitr = 100,
			...)
	}, kmeans = {
		part <- kmeans(as.binary(x), centers = k, nstart = 25)
	}, wards = {
		part <- hclust(dis, method = "ward",
		...)
	}, external = {
		part <- clustering
	}
)
if (inherits(part, "agnes") | inherits(part, "hclust")) {
	grp <- cutree(part, k)
	names(grp) <- rownames(x)
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
	names(grp) <- rownames(x)
}
if (is.vector(part)) {
	grp <- as.numeric(factor(part))
	names(grp) <- rownames(x) # prone to error!
}
if (k != length(unique(grp)) && class(part) != "isopam") {
	warning("did not converge for", k, "partitions",
		"\nset k to", length(unique(grp)))
}
	
#	if method return outgroup try to refine clustering
#	using optimal sil widths (polish == TRUE)
#	may comsume significant cpu time!

out.grp <- any(as.vector(table(grp)) == 1)

if (out.grp) {
	warning("single member groups detected!")
}
if (out.grp || polish) {
	if (verbose) cat("\n... try to resolve using function optsil")
	grp.opt <- optsil(grp, dis, k^2)$clustering
	names(grp.opt) <- rownames(x)
	if (any(as.vector(table(grp.opt)) == 1)) {
		warning("\ndid not succeed in reallocation")
	} else {
		method <- c(method, "optsil")	
		grp <- grp.opt
		cat("\n... successfully 'polished' clustering")
		if (k !=  length(unique(grp))) {
			cat("\n... reset intial k =", k,
				"to k =", length(unique(grp)))
		}
	}	
}

res <- new("VegsoupDataPartition",
	part = grp,
	method = part.meth,
	k = length(unique(grp)),
	dist = dist,
	binary = binary,
	species = as.character(x),
	taxonomy = Taxonomy(x),
	species.long = SpeciesLong(x),
	sites.long = SitesLong(x),
	sites = Sites(x),
	sp.points = SpatialPointsVegsoup(x),
	sp.polygons = SpatialPolygonsVegsoup(x),
	group = AprioriGrouping(x),
	scale = AbundanceScale(x),
	layers = Layers(x))

return(res)
}

#	subsetting method
setMethod("[",
    signature(x = "VegsoupDataPartition",
    i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE)
    {
	    #	debug
	    #	x = prt; i = 1:20; j = 1:20
	    part <- Partitioning(x)
	    
	    if (missing(i)) i <- rep(TRUE, nrow(x))
	    if (missing(j)) j <- rep(TRUE, ncol(x))
	    
	    res <- as(x, "VegsoupData")[i, j]

		if (length(unique(part[names(part) %in% rownames(res)])) != getK(x)) {
			cat("Partitioning vector was subsetted!")
			cat("k was changed accordingly ")
		}
		res <- new("VegsoupDataPartition",
			part = part[names(part) %in% rownames(res)],
			method = x@method,
			k = length(unique(part[names(part) %in% rownames(res)])),
			dist = x@dist,
			binary = x@binary,
			species = res@species,
			sites = res@sites,
			species.long = res@species.long,
			sites.long = res@sites.long,
			taxonomy = res@taxonomy,
			scale = res@scale,
			layers = res@layers,
			group = res@group[names(part) %in% rownames(res)],
			sp.points = res@sp.points,
			sp.polygons = res@sp.polygons)

	    return(res)
    }
)

#	VegsoupDataPartition helper functions

.VegsoupDataPartitionOptpartBestopt <- function (dist, k, numitr, verbose = TRUE) 
{
    if (class(dist) != "dist") {
        stop("bestopt is only defined for objects of class dist")
    }
    best <- 0
    ratios <- rep(0, numitr)
    for (i in 1:numitr) {
    	if (verbose) cat(".")
		tmp <- optpart(k, dist)        
        while (max(tmp$clustering) != k) {
        	tmp <- optpart(k, dist)
        	}
        ratios[i] <- max(tmp$ratio)
        if (ratios[i] > best) {
            best <- ratios[i]
            result <- tmp
            itr <- i
        }
    }
    cat("\nRatios for respective optparts \n")
    print(format(ratios, digits = 4))
    cat(paste("\nChoosing # ", itr, " ratio = ", format(best, 
        digits = 4), "\n"))
    invisible(result)
}

.plotVegsoupPartition  <- function (x, y) {
	#	x = prt
#	op <- par()
#	on.exit(par(op))
	
	cat("\nLet me calculate capscale first ...")
	cat("\nuse distance:", x@dist)

	#	capscale has difficulties when using community matrix in the formula

	ord <- capscale(getDist(prt) ~ 1,
		comm = as.binary(x))
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
			if(class(cents) != "try-error") {
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
}
	
setMethod("plot",
	signature(x = "VegsoupDataPartition", y = "missing"),
	.plotVegsoupPartition
)

#	generic getter methods

#	running partition vector
setGeneric("Partitioning",
	function (obj)
		standardGeneric("Partitioning")
)
setMethod("Partitioning",
	signature(obj = "VegsoupDataPartition"),
	function (obj) obj@part
)

#	retrieve distance matrix
setGeneric("getDist",
	function (obj)
		standardGeneric("getDist")
)
setMethod("getDist",
	signature(obj = "VegsoupDataPartition"),
	function (obj) vegdist(as.binary(obj), obj@dist)
)

#	connectedness of dissimilarities
#	method for class VegsoupDataPartition
setMethod("getDistconnected",
	signature(obj = "VegsoupDataPartition"),
	function (obj, ...) distconnected(getDist(obj), ...)
)

#	number of partitions
setGeneric("getK",
	function (obj)
		standardGeneric("getK")
)
setMethod("getK",
	signature(obj = "VegsoupDataPartition"),
	function (obj) obj@k
)

#	list of species occurence in clusters
setGeneric("Spread",
	function (obj)
		standardGeneric("Spread")
)
setMethod("Spread",
	signature(obj = "VegsoupDataPartition"),
	function (obj) {
		part  <- Partitioning(obj)
		com <- as.binary(obj)
		if (length(unique(part)) == 1)
			warning("only single partition present")
		res <- apply(com, 2, function (y) {
			sapply(rownames(com[y > 0,]), # y
				function (z) part[which(names(part) == z)],
				USE.NAMES = FALSE)
			})
	return(res)
	}
)

.summaryVegsoupDataPartition <- function (object, choice = c("all", "species", "sites", "partition"), ...) {
	if (missing(choice)) choice <- "all"
		choices <- c("all", "species", "sites", "partition")
	choice <- choices[pmatch(choice, choices)]
	if (is.na(choice)) choice <- "all"
	switch(choice, "all" = {
		.summaryVegsoupData(object, choice = "all")
		cat("\ntable of partition contingencies")
		print(table(Partitioning(object)))
	}, "species" = {
		.summaryVegsoupData(object, choice = "species")
	}, "sites" = {
		.summaryVegsoupData(object, choice = "sites")
	}, "partition" = {
		cat("\ntable of partition contingencies")
		print(table(Partitioning(object)))	
	})	
	
}


setMethod("summary",
    signature(object = "VegsoupDataPartition"),
	.summaryVegsoupDataPartition
)

#	contingency table
setGeneric("Contingency",
	function (obj)
		standardGeneric("Contingency")
)
setMethod("Contingency",
	signature(obj = "VegsoupDataPartition"),
	function (obj) {
		res <- t(aggregate(as.binary(obj),
			by = list(Partitioning(obj)), FUN = sum))[-1,]
		colnames(res) <- unique(Partitioning(obj))
		rownames(res) <- names(obj)
		return(res)
	}
)

#	frequency (constancy) table	
setGeneric("Constancy",
	function (obj, ...)
		standardGeneric("Constancy")
)
setMethod("Constancy",
	signature(obj = "VegsoupDataPartition"),
	function (obj, percentage = TRUE, ...) {
		tmp <- t(as.matrix(table(Partitioning(obj))))
		res <- Contingency(obj) / tmp[rep(1, nrow(Contingency(obj))),]
		if (percentage)
			res <- round (res * 100, 0)
		return(res)
	}
)

#	summary statistics

#	Fisher Test
#	depreciated
#	use Fidelity(obj, "Fisher") instead
.FisherTestVegsoupPartition <-  function (obj, alternative = "two.sided") {

#	apapted from isotab.R (package 'isopam')
#	which borrowed by itself from fisher.test

alternative <-  match.arg(as.character(alternative), c("greater","less","two.sided"))

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
		d/sum(d)
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

	p <- switch(alternative,
		less = pnhyper(x, 1),
		greater = pnhyper(x, 1, upper.tail = TRUE),
		two.sided = {
    	        relErr <- 1 + 10^(-7)
        	    d <- dnhyper(1)
            	sum(d[d <= d[x - lo + 1] * relErr])
	        })
	return(p)
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
n.i <- colSums(as.binary(obj))			# species frequencies
N_pi <- table(Partitioning(obj))	# number of plots in partition
n_pi <- Contingency(obj)			# number of occurences in partition

res <- n_pi

for (i in 1:ncol(obj)) { # loop over species
	n <- n.i[i]						# n	
    for (j in 1:length(N_pi)) {		# loop over partitions
		N_p <- N_pi[j]
		n_p <- n_pi[i,j]    	
    	res[i,j] <- FisherPval(ObservedFreqencyTable(N, N_p, n, n_p))
    }
}
 
return(res)
}

setGeneric("FisherTest",
	function (obj, ...)
		standardGeneric("FisherTest")
)
setMethod("FisherTest",
	signature(obj = "VegsoupDataPartition"),
	.FisherTestVegsoupPartition	
)
	
#	standardised Phi statistic
#	depreciated
#	use Fidelity(obj, func = "r.g") instead
.PhiVegsoupPartition <- function (obj) {
	
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

setGeneric("Phi",
	function (obj)
		standardGeneric("Phi")
)
setMethod("Phi",
	signature(obj = "VegsoupDataPartition"),
	.PhiVegsoupPartition	
)

#	Dufrene & Legendre's indicator value
setGeneric("Indval",
	function (obj, ...)
		standardGeneric("Indval")
) 
setMethod("Indval",
	signature(obj = "VegsoupDataPartition"),
	function (obj, ...) {
		res <- indval(as.binary(obj), Partitioning(obj), ...)$indval
		return(res)
	}
)

#	De Cáceres et al. 2010 indicator species analysis by combining groups of sites
setGeneric("Multipatt",
	function (obj, ...)
		standardGeneric("Multipatt")
) 
setMethod("Multipatt",
	signature(obj = "VegsoupDataPartition"),
	function (obj, ...) {
		if (getK(obj) == 1) {
			stop("meaningless with k = ", getK(obj))
		}	
		res <- multipatt(as.binary(obj), Partitioning(obj),
			...)
		return(res)
	}
)

#	Aho et al 2009 indicator value minimizing intermediate occurrences
setGeneric("Isamic",
	function (obj)
		standardGeneric("Isamic")
)
setMethod("Isamic",
	signature(obj = "VegsoupDataPartition"),
	function (obj) { 	
	   	tmp <- Constancy(prt) / 100
    	res <- apply(tmp, 1, function (x) {
    			2*sum(abs(as.numeric(x)- 0.5)) / ncol(tmp)
    		})
    	return(res)
    }
)

#	table deviance
setGeneric("Tabdev",
	function (obj, ...)
		standardGeneric("Tabdev")
)

setMethod("Tabdev",
	signature(obj = "VegsoupDataPartition"),
	function (obj, ...) {
		if (getK(obj) == 1) {
			stop("meaningless with k = ", getK(obj))
		}	
		res <- tabdev(as.binary(obj),
			Partitioning(obj), ...)$spcdev
		return(res)
	}
)

#	optimise partitioning using Dave Roberts optsil procedure
#	see ?optsil for details. maxitr is set to one by default.
setGeneric("Optsil",
	function (obj, ...)
		standardGeneric("Optsil")
)
setMethod("Optsil",
    signature(obj = "VegsoupDataPartition"),
    function (obj, maxit, ...) {
	#	obj  <- prt
		if (!inherits(obj, "VegsoupDataPartition"))
			stop("Need object of class VegsoupDataPartition")
		if (missing(maxit)) {
			maxit <- 1
			cat("set maxit to", maxit)
		}
		if (getK(obj) == 1) {
			stop("meaningless with k = ", getK(obj))
		}	
		res <- obj	
		cpu.time <- system.time({
			res@part <- as.integer(optsil(
			x = Partitioning(obj),
			dist = getDist(obj), ...)$clustering)
		})
	
		cat("\ntime to optimise species matrix",
		"in", getK(obj), "partitions:",
		cpu.time[3], "sec\n")
		
		names(res@part) <- names(obj@part)
		return(invisible(res))
	}
)

#	optimise partitioning by Dufrene & Legendre's indicator value
setGeneric("Optindval",
	function (obj, ...)
		standardGeneric("Optindval")
)
setMethod("Optindval",
    signature(obj = "VegsoupDataPartition"),
    function (obj, ...) {
    	if (getK(obj) == 1) {
			stop("meaningless with k = ", getK(obj))
		}	
		res <- obj
		cat("run optimisation")
		cpu.time <- system.time({
		res@part <- as.integer(optindval(as.binary(obj),
			Partitioning(obj), ...)$clustering)
		})
		
		cat("\ntime to optimise species matrix",
		"in", getK(obj), "partitions:",
		cpu.time[3], "sec\n")
					
		names(res@part) <- names(obj@part)
		return(invisible(res))
    }
)

#	indicator species analysis by Murdoch preference function
setGeneric("Murdoch",
	function (obj, ...)
		standardGeneric("Murdoch")
)
setMethod("Murdoch",
    signature(obj = "VegsoupDataPartition"),
    function (obj, minplt, type, ...) {
    	if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))
    	if (missing(minplt))
    		minplt <- 1
		if (missing(type))
			part <- Partitioning(obj)
		ks <- getK(obj)
		res <- matrix(0, nrow = dim(obj)[2], ncol = ks)
		dimnames(res) <- list(names(obj), 1:ks)
		pval <- matrix(0, nrow = dim(obj)[2], ncol = ks)
		dimnames(pval) <- list(names(obj), 1:ks)
		res.ls <- vector("list", length = ks)
		names(res.ls) <- 1:ks
		for (i in 1:ks) {
			res.ls[[i]] <- murdoch(as.binary(obj),
				part == i, minplt = minplt)
			res[,i] <- res.ls[[i]]$murdoch
			pval[,i] <- round(res.ls[[i]]$pval, 3)
		}
    	return(c(res.ls, list(murdoch = res, pval = pval)))
    }
)

#	Partition Analysis
setGeneric("Partana",
	function (obj, ...)
		standardGeneric("Partana")
)

setMethod("Partana",
    signature(obj = "VegsoupDataPartition"),
    function (obj, method , ...) {
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))
    	if (inherits(obj, "VegsoupDataPartition")) {
    		dis <- getDist(obj)
    	} else {    	
			if (missing(method)) {    			
				dis <- vegdist(as.binary(obj), "bray")
    		} else {
    			dis <- vegdist(as.binary(obj), ...)
    		}
    	}	    	
		res <- partana(Partitioning(obj), dis)
		return(res)    	
    }
)

#	Silhouette Analysis
setGeneric("Silhouette",
	function (obj, ...)
		standardGeneric("Silhouette")
)

setMethod("Silhouette",
    signature(obj = "VegsoupDataPartition"),
    function (obj, method , ...) {
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))
    	if (inherits(obj, "VegsoupDataPartition")) {
    		dis <- getDist(obj)
    	} else {    	
			if (missing(method)) {    			
				dis <- vegdist(as.binary(obj), "bray")
    		} else {
    			dis <- vegdist(as.binary(obj), ...)
    		}
    	}	    	
		res <- silhouette(Partitioning(obj), dis)
		return(res)    	
    }
)

#	identification of typal samples in a partition
setGeneric("Typal",
	function (obj)
		standardGeneric("Typal")
)
setMethod("Typal",
    signature(obj = "VegsoupDataPartition"),
    function (obj) {
		if (getK(obj) == 1) {
			warning("results are meaningless with k = ", getK(obj))
			return(invisible(obj))
		} else {
   			res <- typal(Partitioning(obj), getDist(obj))
	   		return(invisible(res))
   		}
    }
)

#	'head' like summary function based on
#	identification of typal samples in a partition
#	method for class VegsoupDataPartition
setMethod("head",
    signature(x = "VegsoupDataPartition"),
    function (x, n = 6L, choice = "species", ...) {
    	if (missing(choice))
	    	choice <- "species"
	    if (n != 6L) {
	    	sel <- match(c(as.matrix(Typal(x)$silhouette)),
	    		rownames(x))
	    	if (choice == "species")
    			res <- as.character(x)[sel,]
	    	if (choice == "sites")
    			res <- Sites(x)[sel,]
    	} else {
	    	if (choice == "species")
    			res <- head(x@species, ...)
	    	if (choice == "sites")
    			res <- head(x@sites, ...)
    	}	
    	return(res)
    }    	    
)

#	dissimilarity diameters
			
setGeneric("Disdiam",
	function (obj, ...)
		standardGeneric("Disdiam")
)

setMethod("Disdiam",
    signature(obj = "VegsoupDataPartition"),
    function (obj, method, ...) {
		if (getK(obj) == 1)
			stop("meaningless with k = ", getK(obj))
    	if (inherits(obj, "VegsoupDataPartition")) {
    		dis <- getDist(obj)
    	} else {    	
			if (missing(method)) {    			
				dis <- vegdist(as.binary(obj), "bray")
    		} else {
    			dis <- vegdist(as.binary(obj), ...)
    		}
    	}	    	
		res <- disdiam(Partitioning(obj), dis)
		return(res)    	
    }
)

#	confusion matrix to compare two clusterings
setGeneric("Confus",
	function (obj1, obj2)
		standardGeneric("Confus")
)

setMethod("Confus",
    signature(obj1 = "VegsoupDataPartition",
    	obj2 = "VegsoupDataPartition"),
	function (obj1, obj2) {
	#	obj1 = prt; obj2 = prt.opt
		cls <- Partitioning(obj1)
    	N <- length(cls)
	    nc <- length(table(cls))
    	cmp <- Partitioning(obj2)
		res <- table(cls, cmp)
    	crt <- sum(diag(res))	# correct
	    per <- crt / N			# percent
		sum <- sum(rowSums(res) * colSums(res))
	    kap <- (c(N * crt) - sum) / (N^2 - sum)
    	res <- list(confus = res,
    		correct = crt,
    		percent = per, 
        	kappa = kap)
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
			function(x) as.numeric(x == levels(factor(Partitioning(obj))))))
		dimnames(res)[2] = list(levels(factor(Partitioning(obj))))
    return(res)                                                                                                                              
	}
)