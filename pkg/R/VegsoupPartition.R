#	generating function
#	to do: implement formula interface for method, high priority
VegsoupPartition <- function (obj, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "external"), clustering, polish = FALSE, seed = 1234, verbose = FALSE, ...) {

#	debug
#	obj = dta; k = 3;
#	method = "isopam"
#	dist = "bray"
#	nitr = 99; polish = TRUE
#	method = "external"
#	clustering = "syntaxon"
	CALL <- match.call()
	
	if (!inherits(obj, "Vegsoup")) {
		stop("Need object of class Vegsoup")
	}
	#	check second and thrid unnamed arguments
#	if (!is.numeric(CALL$k)) {
#		stop("second unnamed argument is not numeric")
#	}
#	if (!is.character(CALL$method)) {
#		stop("third unnamed argument is not a character")
#	}
	#	for class(obj) VegsoupOptimstride
	if ((missing(k) & missing(clustering)) & !inherits(obj, "VegsoupOptimstride")) {
		k = 1
		warning(" argument k missing, set to ", k, call. = FALSE)
	}	
	if (missing(k) & inherits(obj, "VegsoupOptimstride")) {
		#	warning! no sensible results so far!
		k = summary(opt)$best.optimclass1
	}
	#	for class(obj) Vegsoup
	if (missing(k) & missing(clustering)) {
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
			
	#	species and distance matrices
	X <- as.matrix(obj)	
	if (part.meth != "external") {	
		Xd <- as.dist(obj)
	}

	#	print settings before run
	if (verbose) {
		cat("\n run with settings",
		    "\n k:", k,
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
		part <- isopam(X, distance = vegdist(obj),
			...)
	}, optpart = {
		if (verbose) cat("\nrun optpart from random starts ...")
		if (verbose) cat("\nset to", k, "partitions\n")
		part <- .VegsoupPartitionOptpartBestopt(Xd, k, numitr = 100,
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

#	develop class VegsoupPartition from class Vegsoup
res <- new("VegsoupPartition", obj)
#	assign class slots
res@part <- grp
res@method <- part.meth	
res@k <- length(unique(grp))

return(res)
}
