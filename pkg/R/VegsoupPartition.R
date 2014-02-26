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
		M <- method <- "flexible"	
		if (verbose) cat("... Set default option", M)
	}
	else {
		METHODS <- c("ward", "flexible", "pam", "isopam",
					 "kmeans", "optpart", "wards", "fanny",
					 "FCM", "KM", # vegclust
					 "external")
		M <- match.arg(method, METHODS)
	}
	if (!missing(clustering) | match.arg(method) == "external") {
		if (missing(clustering)) {
			message("selected method external but clustering is misssing")
		}
		M <- method <- "external"
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
	if (M != "external") { # save time
		Xd <- as.dist(obj)
	}

	#	print settings before run
	if (verbose) {
		cat("\n run with settings",
		    "\n k (length of stride) :", k,
			"\n disimilarity measure :", vegdist(obj),
			"\n decostand method     :", decostand(obj),
			"\n partitioning method  :", M, "\n")	
	}	
	
	if (k > 1) {
	#	partitioning methods	
	switch(M,
		   ward = {
		   	P <- agnes(Xd, method = "ward",
		   		keep.diss = FALSE, keep.data = FALSE) # save time and memory
		   		#	no meaningful additional arguments, because of inherits(Xd, "dist")
		   		#	method is defined by switch itself,
		   		#	there is switch flexible for setting par.method		   		
		   		#	, ...)	   
		 }, flexible = {
		   	alpha <- 0.625
		   	beta = 1 - 2 * alpha
		   	P <- agnes(Xd, method = "flexible",
		   		keep.diss = FALSE, keep.data = FALSE, # save time and memory
		   		par.method = c(alpha, alpha, beta, 0))
		   		#	as.above because of inherits(Xd, "dist")
		   		#	, ...) 
		}, pam = {
			P <- pam(Xd, k = k, diss = TRUE)
		   		#	as above, exept do.swap = FALSE
				#	, ...)
		}, isopam = {
			if (verbose) cat("\nrun isopam, ignoring k=", k)
			if (verbose) cat("\nplease supply c.fix to restict to a specific number of partitions\n")
			P <- isopam(X, distance = Xd)
				#	complicated!
				#	, ...)
		}, optpart = {
			if (verbose) cat("\nrun optpart from random starts ...")
			if (verbose) cat("\nset to", k, "partitions\n")
			P <- .VegsoupPartitionOptpartBestopt(Xd, k, numitr = 100)
				#	, ...)
		}, kmeans = {
			if (verbose) cat("kmeans doesn't use distance matrices, ignore", vegdist(obj))
			P <- kmeans(X, centers = k)
				#	iter.max = 10, nstart = 1, algorithm and irgnore trace
				#	, ...)		
		}, wards = {
			P <- hclust(Xd, method = "ward")
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
			m <- ifelse(any(names(CALL) == "memb.exp"), CALL$memb.exp, 1.1)			
			P <- fanny(Xd, k = k, diss = TRUE, memb.exp = m,
				# in any case, we save memory allocation time here
				cluster.only = TRUE, keep.diss = FALSE, keep.data = FALSE)
				#	irgnore: diss and k 
				#	stand = FALSE, irgnore we get standardisation from obj
				#	should accept: iniMem.p = NULL, maxit = 500, tol = 1e-15					
				#	, ...)							
		}, FCM = {
			#	as with fanny()	we need to take care for m (membership exponent)
			m <- ifelse(any(names(CALL) == "m"), CALL$m, 1.1)
			P <- vegclustdist(Xd, mobileMemb = k, method = "FCM", m = m)
			#	more options available
			#	, ...)
		}, KM = {
			P <- vegclustdist(Xd, mobileMemb = k, method = "KM")
			#	more options available
			#	, ...)	
		}, external = {
			P <- clustering
		}
	)
	}
	else {
	  P <- NULL
	  M <- ""	
	} # end if (k > 1)
	
	#   handle k = 1
	if (inherits(P, "NULL")) {
		G <- rep(1, nrow(obj))
		names(G) <- rownames(obj)
	}	
	#	otherwise retrieve partitioning vector from methods
	if (inherits(P, "agnes") | inherits(P, "hclust")) {
		G <- cutree(P, k)
		names(G) <- rownames(obj)
	}
	if (inherits(P, "fanny")) {
		G <-  P$clustering
		names(G) <- rownames(obj)
		#	save memberhsip matrix
		mm <- P$membership
		colnames(mm) <- paste0("M", 1:k)
		Sites(obj) <- cbind(Sites(obj), mm)
	}
	if (inherits(P, "vegclust")) { # & M == "FCM"
		G <- as.numeric(as.factor(defuzzify(P)$cluster))
		if (length(unique(G)) != k) {
		G <- as.numeric(as.factor(defuzzify(P, method = "cut", alpha = 0.5)$cluster))
		names(G) <- rownames(obj)
		}		
	}	
	if (inherits(P, "kmeans")) {
		G <- P$cluster
	}
	if (inherits(P, "pam")) {
		G <- P$clustering
	}
	if (inherits(P, "isopam")) {
		if (is.null (P$hier)) {
			if (verbose) cat("no hierarchy estimated by isopam")
		  	G <- P$flat
		  	k  <- length(unique(P$flat))
		} else { 
			if (verbose) cat("retieve lowest hierachy level")
			G <- P$flat[[ncol(P$hier)]]
			k <- length(unique(G))
		}
	}
	if (inherits(P, "partana")) {
		G <- P$clustering
		names(G) <- rownames(obj)
	}
	if (is.vector(P)) { # method external
		G <- as.numeric(factor(P))
		# warning: prone to error if clustering is not selected from sites!
		names(G) <- rownames(obj)
		if (verbose) {
			print(data.frame(clustering = levels(factor(clustering)),
				assigned = as.numeric(factor(levels(factor(clustering)))) )
			)			
		}
	}
	if (k != length(unique(G)) && class(P) != "isopam") {
		message(M, "did not converge for ", k, " partitions")#,
			# "try setting k to ", length(unique(G))
	}
		
	#	if method returns singleton try to refine clustering
	#	using optimal sil widths (polish = TRUE)
	#	may comsume significant cpu time!	
	out.G <- any(as.vector(table(G)) == 1)
	
	if (verbose) {
		if (out.G) message("single member groups detected!")
	} 
	if (out.G && polish) {
		if (verbose) cat("\n... try to resolve using function optsil")
		#	Imports: optpart
		#	require(optpart)
		G.opt <- optpart::optsil(G, Xd, k^2)$clustering
		names(G.opt) <- rownames(obj)
		if (any(as.vector(table(G.opt)) == 1)) {
			message("did not succeed in reallocation")
		}
		else {
			method <- c(method, "optsil")	
			G <- G.opt
			#	cat("\n successfully 'polished' clustering")
			#	if (k != length(unique(G))) {
			#		cat("\n reset intial k =", k,
			#			"to k =", length(unique(G)), "\n")
			#	}
		}	
	}
	
	#	develop class VegsoupPartition from class Vegsoup
	res <- new("VegsoupPartition", obj)
	#	assign class slots
	res@part <- G
	res@method <- M	
	res@k <- length(unique(G))
	
	return(res)
}
