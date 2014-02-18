#	warning! some how slot sp.points can get messed up?
OptimStride <- function (x, k, ft.threshold = 1e-3, alternative = "two.sided", method = c("ward", "flexible", "pam", "kmeans", "wards", "fanny", "FCM", "KM"), fast = FALSE, ...) {
	if (missing(k)) {
		stop("please supply k for stride")
	}
	else {
		if (k > nrow(x)) {
			k = nrow(x) - 1
			warning("k can't exceed number of plots, set k to nrow(x) - 1")
		}
	}	
	stopifnot(inherits(x, "Vegsoup"))

	cycle <- function (x, k, ...) {
		prt <- VegsoupPartition(x, k = k, ...)
		ft <- FisherTest(prt, alternative = alternative)
		res <- apply(ft < ft.threshold, 2, sum)
		return(res)
	}
	
	if (as.logical(fast)) {
		require(parallel)
		message("fork multicore process on ", parallel::detectCores(), " cores")
	}	
	
	#	results list for top level loop
	res.i <- vector("list", length = length(method))
	names(res.i) <- method
	
	for (i in seq(along = method)) {
		if (fast) {
			cat(method[i], " ")			
			res.j <- mclapply(2:k, function (y, ...) cycle(x, k = y, method = method[i], ...), ...)
			res.i[[i]] <- c(0, res.j)
		} else {
			res.j <- vector("list", length = k)
	 		names(res.j) <- 1:k
			res.j[[1]] <- 0
			names(res.j[[1]]) <- 1
			
			print("\n")
			pb.j <- txtProgressBar(min = 2, max = k,
			char = '.', width = 45, style = 3)			
			for (j in 2:k) {
				setTxtProgressBar(pb.j, j)
				res.j[[j]] <- cycle(x, k = j, method = method[i], ...)
			}
			res.i[[i]] <- res.j
			close(pb.j)
			print("\n")
		}
		
	}	
	
	#	develop class VegsoupOptimstride
	res <- new("VegsoupOptimstride", x)
	
	os <- list(
		indicators = res.i,
		settings = list(call = match.call(),
			args = c(as.list(match.call())[-c(1,2)])))
	os$settings$args$method <- method
	os$settings$args$ft.threshold <- ft.threshold
	os$settings$args$alternative <- alternative

	res@optimstride <- os
	
	#	report if any significant indicator
	#	for at least one of the groups was found
	if (sum(unlist(os$indicators)) == 0) {
		warning("ft.threshold of ", ft.threshold, " seems to be too low?")
	}
	return(res)
}
