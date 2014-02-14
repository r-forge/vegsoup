#	generating function
#	to do: implement formula interface for method, high priority
VegsoupPartition <- function (obj, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "fanny", "FCM", "KM", "external"), clustering, polish = FALSE, seed = 1234, verbose = FALSE, ...) {

	#	debug
	#	obj = dta; k = 3;
	#	method = "isopam"
	#	dist = "bray"
	#	nitr = 99; polish = TRUE
	#	method = "external"
	#	clustering = "syntaxon"
	CALL <- match.call()
	#	set seed
	set.seed(seed)
				
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
		message("argument k missing, set to ", k)
	}	
	if (missing(k) & inherits(obj, "VegsoupOptimstride")) {
		#	warning! no sensible results so far!
		k = summary(obj)$best.optimclass1
	}
	#	for class "Vegsoup"
	if (missing(k) & missing(clustering)) {
		stop("Need a value of k or optional clustering vetcor")
	}
	if (missing(method) & missing(clustering)) {
	#	function default	
		part.meth <- method <- "flexible"	
		if (verbose) {
			cat("... Set default option", part.meth)
		}	
	}
	else {
		METHODS <- c("ward", "flexible", "pam", "isopam",
					 "kmeans", "optpart", "wards", "fanny",
					 "FCM", "KM", # vegclust
					 "external")
		part.meth <- match.arg(method, METHODS)
	}
	if (!missing(clustering) | match.arg(method) == "external") {
		if (missing(clustering)) {
			message("selected method external but clustering is misssing")
		}
		part.meth <- method <- "external"
		if (length(clustering) == 1) {		
			sel <- pmatch(clustering, names(obj))
			if (!is.na(sel)) {			
				clustering <- as.vector(Sites(obj)[, sel]) 			
			}
			else {
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
		}
		else {
			stop("... length of clustering vector",
				" and nrow(obj) must match",
				dim(obj), length(clustering))
		}
	}
			
	#	species and distance matrices
	X <- as.matrix(obj)	
	if (part.meth != "external") { # save time
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
	
	#	partitioning methods	
	switch(part.meth,
		   ward = {
		   	part <- agnes(Xd, method = "ward",
		   		keep.diss = FALSE, keep.data = FALSE) # save time and memory
		   		#	no meaningful additional arguments, because of inherits(Xd, "dist")
		   		#	method is defined by switch itself,
		   		#	there is switch flexible for setting par.method		   		
		   		#	, ...)	   
		 }, flexible = {
		   	alpha <- 0.625
		   	beta = 1 - 2 * alpha
		   	part <- agnes(Xd, method = "flexible",
		   		keep.diss = FALSE, keep.data = FALSE, # save time and memory
		   		par.method = c(alpha, alpha, beta, 0))
		   		#	as.above because of inherits(Xd, "dist")
		   		#	, ...) 
		}, pam = {
			part <- pam(Xd, k = k, diss = TRUE)
		   		#	as above, exept do.swap = FALSE
				#	, ...)
		}, isopam = {
			if (verbose) cat("\nrun isopam, ignoring k=", k)
			if (verbose) cat("\nplease supply c.fix to restict to a specific number of partitions\n")
			part <- isopam(X, distance = vegdist(obj))
				#	complicated!
				#	, ...)
		}, optpart = {
			if (verbose) cat("\nrun optpart from random starts ...")
			if (verbose) cat("\nset to", k, "partitions\n")
			part <- .VegsoupPartitionOptpartBestopt(Xd, k, numitr = 100)
				#	, ...)
		}, kmeans = {
			if (verbose) cat("kmeans doesn't use distance matrices, ignore", vegdist(obj))
			part <- kmeans(X, centers = k)
				#	iter.max = 10, nstart = 1, algorithm and irgnore trace
				#	, ...)		
		}, wards = {
			part <- hclust(Xd, method = "ward")
				#	members != NULL, dissimilarity matrix between clusters
				#	instead of dissimilarities between singletons is not
				#	meaningful
				#	, ...)
		}, fanny = {
			#	the value of memb.exp is an issue with fanny()
			#	we lower default value to prevent fanny()
			#	complaining about "memberships are all very close to 1/k"
			#	we keep the value very crisp at 1.1
			#	was: length(grep("memb.exp", deparse(CALL), fixed = TRUE)) < 1
			memb.exp <- ifelse(any(names(CALL) == "memb.exp"), CALL$memb.exp, 1.1)			
			part <- fanny(Xd, k = k, diss = TRUE, memb.exp = memb.exp,
				# in any case, we save memory allocation time here
				cluster.only = TRUE, keep.diss = FALSE, keep.data = FALSE)
				#	irgnore: diss and k 
				#	stand = FALSE, irgnore we get standardisation from obj
				#	should accept: iniMem.p = NULL, maxit = 500, tol = 1e-15					
				#	, ...)							
		}, FCM = {
			#	as with fanny()	we need to take care for m (membership exponent)
			m <- ifelse(any(names(CALL) == "m"), CALL$m, 1.1)
			part <- vegclustdist(Xd, mobileMemb = k, method = "FCM", m = m)
			#	more options available
			#	, ...)
		}, KM = {
			part <- vegclustdist(Xd, mobileMemb = k, method = "KM")
			#	more options available
			#	, ...)	
		}, external = {
			part <- clustering
		}
	)

	#	retrieve partitioning vector from methods
	if (inherits(part, "agnes") | inherits(part, "hclust")) {
		grp <- cutree(part, k)
		names(grp) <- rownames(obj)
	}
	if (inherits(part, "fanny")) {
		grp <-  part$clustering
		names(grp) <- rownames(obj)
		#	save memberhsip matrix
		mm <- part$membership
		colnames(mm) <- paste0("M", 1:k)
		Sites(obj) <- cbind(Sites(obj), mm)
	}
	if (inherits(part, "vegclust")) { # & part.meth == "FCM"
		grp <- as.numeric(as.factor(defuzzify(part)$cluster))
		if (length(unique(grp)) != k) {
		grp <- as.numeric(as.factor(defuzzify(part, method = "cut", alpha = 0.5)$cluster))
		names(grp) <- rownames(obj)
		}		
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
		message("did not converge for ", k, " partitions")#,
			# "try setting k to ", length(unique(grp))
	}
		
	#	if method returns singleton try to refine clustering
	#	using optimal sil widths (polish = TRUE)
	#	may comsume significant cpu time!	
	out.grp <- any(as.vector(table(grp)) == 1)
	
	if (verbose) {
		if (out.grp) message("single member groups detected!")
	} 
	if (out.grp && polish) {
		if (verbose) cat("\n... try to resolve using function optsil")
		#	Imports: require(optpart)
		grp.opt <- optpart::optsil(grp, Xd, k^2)$clustering
		names(grp.opt) <- rownames(obj)
		if (any(as.vector(table(grp.opt)) == 1)) {
			message("did not succeed in reallocation")
		}
		else {
			method <- c(method, "optsil")	
			grp <- grp.opt
			#	cat("\n successfully 'polished' clustering")
			#	if (k != length(unique(grp))) {
			#		cat("\n reset intial k =", k,
			#			"to k =", length(unique(grp)), "\n")
			#	}
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
