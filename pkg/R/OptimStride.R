#	warning! some how slot sp.points cab get messed up?
OptimStride <- function (obj, k, ft.treshold = 1e-3, alternative = "two.sided", method = c("ward", "flexible", "pam", "kmeans", "wards"), CALL = match.call(), ...) {
	if (missing(k)) stop("please supply k for stride")
	#	obj = dta; k = 3; alternative = "greater"
	cycle <- function (obj, k, ...) {
		prt <- VegsoupPartition(obj, k = k, ...)
		ft <- FisherTest(prt, alternative = alternative)
		res <- apply(ft < ft.treshold, 2, sum)
		return(res)
	}

	res.i <- vector("list", length = length(method))
	names(res.i) <- method
#	print(method)
	
	for (i in seq(along = method)) {
#		cat(method[i])
		res.j <- vector("list", length = k)
		names(res.j) <- 1:k
		res.j[[1]] <- 0
		names(res.j[[1]]) <- 1
#		since R 2.15.2 (2012-10-26)
#		txtProgressBar behaves somehow differnt? Mac Gui?
		print("\n")
		pb.j <- txtProgressBar(min = 2, max = k,
			char = '.', width = 45, style = 3)		
		for (j in 2:k) {
			setTxtProgressBar(pb.j, j)
			res.j[[j]] <- cycle(obj, method = method[i], k = j, ...)
		}
		res.i[[i]] <- res.j
		close(pb.j)
		print("\n")
	}	
	
#	very slow from here?
	#	develop class VegsoupOptimstride
	res <- new("VegsoupOptimstride", obj)
	
	os <- list(
		indicators = res.i,
		settings = list(call = match.call(),
			args = c(as.list(match.call())[-c(1,2)])))
	os$settings$args$method <- method
	os$settings$args$ft.treshold <- ft.treshold
	os$settings$args$alternative <- alternative

	res@optimstride <- os
	
	#	report if any significant indicator
	#	for at least one of the groups was found
	if (sum(unlist(os$indicators)) == 0) {
		warning("ft.treshold of ", ft.treshold, " seems to be too low?")
	}
	return(res)
}
