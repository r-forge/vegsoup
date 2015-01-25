#	warning! some how slot sp.points can get messed up?
OptimStride <- function (x, k, ft.threshold = 1e-3, alternative = "two.sided", method = c("ward", "flexible", "pam", "kmeans", "wards", "fanny", "FCM", "KM"), fast = FALSE, ...) {

	stopifnot(inherits(x, "Vegsoup"))

	CALL <- match.call()
		
	#	species and distance matrices
	X <- as.matrix(x)
	Xd <- as.dist(x)
	
	if (missing(k)) { 
		stop("please supply k for stride")
	}
	else {
		if (k > nrow(x)) {
			k = nrow(x) - 1
			warning("k can't exceed number of plots, set k to nrow(x) - 1")
		}
	}	

	if (as.logical(fast)) {
		#	parallel is in imports
		message("fork multicore process on ", parallel::detectCores(), " cores")
	}	

	#	supply matrix and dist argument to speed up
	#	we don't need to calculate this in each step
	cycle <- function (x, k, X, Xd, ...) {
		P <- VegsoupPartition(x, k = k, X = X, Xd = Xd, ...)
		r <- FisherTest(P, alternative = alternative)
		r <- apply(r < ft.threshold, 2, sum)
		return(r)
	}
		
	#	results list for top level loop
	ri <- vector("list", length = length(method))
	if (inherits(method, "function"))
		#	keep in sync with VegsoupPartition
		M <- paste(CALL$method, "<-", deparse(method)[1])
	else
		M <- method
	names(ri) <- M
	
	for (i in seq(along = method)) {
		if (inherits(method, "function"))
			m <- list(method)[[ i ]]
		else
			m <- method[ i ]
		
		if (as.logical(fast)) {
			message(M[ i ], " ")
			rj <- mclapply(2:k, function (y, ...) cycle(x, k = y, method = m, X = X, Xd = Xd, ...), ...)
			ri[[ i ]] <- c(0, rj)
		}
		else {
			rj <- vector("list", length = k)
			names(rj) <- 1:k
			rj[[ 1 ]] <- 0
			names(rj[[ 1 ]]) <- 1

			pb.j <- txtProgressBar(min = 2, max = k, char = '.', width = 45, style = 3)
			for (j in 2:k) {
				setTxtProgressBar(pb.j, j)
				rj[[ j ]] <- cycle(x, k = j, method = m, , X = X, Xd = Xd, ...)
			}
			ri[[ i ]] <- rj
			close(pb.j)
		}	
	}	
	
	#	develop class VegsoupOptimstride
	r <- new("VegsoupOptimstride", x)
	
	os <- list(
		indicators = ri,
		settings = list(call = match.call(),
			args = c(as.list(match.call())[-c(1,2)])))
	os$settings$args$method <- M
	os$settings$args$ft.threshold <- ft.threshold
	os$settings$args$alternative <- alternative

	r@optimstride <- os
	
	#	report if any significant indicator
	#	for at least one of the groups was found
	if (sum(unlist(os$indicators)) == 0) {
		warning("ft.threshold of ", ft.threshold, " seems to be too low?")
	}
	
	return(r)
}
