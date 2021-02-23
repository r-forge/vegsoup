#	to do: implement formula interface for method, high priority

#	low level function to allow distance matrix as argument Xd and raw species matrix as argument X
#	this speeds up calculation in OptimStride() 
.VegsoupPartition <- function (obj, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "fanny", "FCM", "KM", "external"), clustering, polish = FALSE, seed = NULL, verbose = FALSE, X, Xd, ...) {

	CALL <- match.call()
	
	#	set seed
	if (!is.null(seed)) set.seed(seed)
	
	if (!inherits(obj, "Vegsoup"))	stop("Need object of class Vegsoup")

	#	for class(obj) VegsoupOptimstride
	if ((missing(k) & missing(clustering)) & !inherits(obj, "VegsoupOptimstride")) {
		k = 1
		message("argument k missing, set to ", k)
	}	
	if (missing(k) & inherits(obj, "VegsoupOptimstride")) {
		k = summary(obj)$best.optimclass1
	}

	#	for class "Vegsoup"
	if (missing(k) & missing(clustering)) {
		stop("Need a value of k or optional clustering vetcor")
	}

	#	test function arguments
	if (missing(method) & missing(clustering)) {
		# the default
		M <- method <- "flexible"
		if (verbose) cat("... Set default option", M)
	}
	else {
		if (inherits(method, "function")) {
			M <- "FUN"
			expr <- method
		}
		else {
			# what we have in the function signature
			METHODS <- c("ward", "flexible", "pam", "isopam",
						 "kmeans", "optpart", "wards", "fanny",
						 "FCM", "KM", "external")
			M <- match.arg(method, METHODS)
		}
	}

	#	tests method external and argument clustering
	if (M != "FUN") {
		if (!missing(clustering) | match.arg(method) == "external") {
			if (missing(clustering))
				message("selected method external but clustering is misssing")

			M <- method <- "external"
			
			if (length(clustering) == 1) {
				sel <- pmatch(clustering, names(obj))
				if (!is.na(sel)) {
					clustering <- as.vector(sites(obj)[, sel])
				}
				else {
					stop("if length of clustering is 1",
					" the argument has to match a column name of sites(obj)")
				}
			}
			
			if (length(clustering) == nrow(obj)) {
				k <- length(unique(clustering))
				if (verbose) {
					cat("use supplied vector, number of partitons ",
						ifelse(is.integer(k), k, as.integer(k)))
				}	
			}
			else {
				stop("length of clustering vector and nrow(obj) must match",
					dim(obj), length(clustering))
			}
		}
	}

	#	species and distance matrices
	#	use supplied arguments if available
		
	if (any(names(CALL) == "X")) { #missing(X)
		stopifnot(inherits(X, "matrix"))	# message("use X")
	}
	else {
		X <- as.matrix(obj)
	}	
	
	if (M != "external") { # need Xd
		if (any(names(CALL) == "Xd")) {
			stopifnot(inherits(Xd, "dist")) # message("use Xd")
		}
		else {
			Xd <- as.dist(obj) # saves time
		}
	}
	
	
	#	method switch
	if (k > 1) {
		switch(M,
			ward = {
				P <- agnes(Xd, method = "ward",
					keep.diss = FALSE, keep.data = FALSE) # save time and memory
					#	no meaningful additional arguments, because of inherits(Xd, "dist")
					#	method is defined by switch itself,
					#	there is switch flexible for setting par.method
					#	, ...)
			}, flexible = {
				#	the commonly used value of beta (0,25)
				a <- 0.625     # alpha
				b <- 1 - 2 * a # beta
				P <- agnes(Xd, method = "flexible",
					keep.diss = FALSE, keep.data = FALSE, # save time and memory
					par.method = c(a, a, b, 0))
					#	as.above because of inherits(Xd, "dist")
					#	, ...)
			}, pam = {
				P <- pam(Xd, k = k, diss = TRUE)
					#	as above, exept do.swap = FALSE
					#	, ...)
			}, isopam = {  # performs not very well with high levels of k
				if (verbose) cat("\nrun isopam, ignoring k=", k)
				if (verbose) cat("\nplease supply c.fix to restict to a specific number of k\n")
				P <- isopam(X, distance = Xd)
					#	complicated!
					#	, ...)
			}, optpart = {
				# OPTPART/FLEX, initialize with flexible beta												
				alpha <- 0.625
				beta = 1 - 2 * alpha
				P <- agnes(Xd, method = "flexible",
					keep.diss = FALSE, keep.data = FALSE, # save time and memory
					par.method = c(alpha, alpha, beta, 0))				

				G <- cutree(P, k)
				P <- optpart::optpart(G, Xd)
					#	shoud accept: maxitr = 100, mininc = 0.001, maxdmu = 1
					#	, ...)
			}, kmeans = {
				if (verbose) cat("kmeans doesn't use distance matrices, ignore", vegdist(obj))
				P <- kmeans(X, centers = k)
					#	could accept: iter.max = 10, nstart = 1, algorithm
					#	, ...)
			}, wards = {
				P <- hclust(Xd, method = "ward.D")
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
					#	ignore: diss and k 
					#	stand = FALSE, we get standardisation from obj
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
			}, FUN = {
				P <- do.call(expr, args = list(Xd = Xd, X = X, k = k))
			}
		)
	}
	else {
		P <- NULL
		M <- ""
	} # end if (k > 1)

	#	retrieve partitioning vector
	#	for methods in the function signature
	if (M != "FUN") {
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
			sites(obj) <- cbind(sites(obj), mm)
		}
		if (inherits(P, "vegclust")) { # & M == "FCM"
			G <- as.numeric(as.factor(defuzzify(P)$cluster))
			if (length(unique(G)) != k) {
			G <- as.numeric(as.factor(defuzzify(P, method = "cut", alpha = 0.5)$cluster))
			names(G) <- rownames(obj)
			}
			if (is.null(names(G))) names(G) <- rownames(obj) # for KM!
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
			message(M, "did not converge for ", k, " partitions")
		}
	}
	#	for function interface
	else {
		G <- P
		if (length(G) != nrow(obj)) {
			if (length(names(G)) > 0) {
				warning("lost ", nrow(obj) - length(G), " plots", call. = FALSE)
				obj <- obj[match(names(G), rownames(obj)), ]
			}
			else {
				stop("result of FUN should have names if subsetted")
			}
		}
		else {
			#! might be an issue if FUN permutes order
			names(G) <- rownames(obj)
		}
	}	
	#	if method returns singleton try to refine clustering
	#	using optimal sil widths (polish = TRUE)
	#	may comsume significant cpu time!	
	out.G <- any(as.vector(table(G)) == 1)
	
	if (verbose) {
		if (out.G) message("single member groups detected!")
	} 
	if (out.G && polish) {
		if (verbose) cat("\n... try to resolve using function remos")
		#	Imports: optpart
		G.opt <- optpart::optsil(G, Xd, k^2)$clustering
		names(G.opt) <- rownames(obj)
		if (any(as.vector(table(G.opt)) == 1)) {
			message("did not succeed in reallocation")
		}
		else {
			method <- c(method, "optsil")
			G <- G.opt
		}
	}
	
	#	develop class VegsoupPartition from class Vegsoup
	r <- new("VegsoupPartition", obj)
	#	assign class slots
	r@part <- G
	r@partitioning.method <- ifelse(M != "FUN", M, paste(CALL$method, "<-", deparse(method)[1]))
	r@k <- length(unique(G))
	
	return(r)
}

#	exported function
VegsoupPartition <- function (obj, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "fanny", "FCM", "KM", "external"), clustering, polish = FALSE, seed = NULL, verbose = FALSE, ...) {
	
	.VegsoupPartition(obj = obj, k = k, method = method, clustering = clustering, polish = polish,
		seed = seed, verbose = verbose, ...)
}
