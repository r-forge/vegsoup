


#	generating function
#	obj an object of class VegsoupData
#	k number of clusters to create
#	ft.treshold treshold value of Fisher test to defauklts to 1e-03
#	alternative indicates the alternative hypothesis of Fisher exact test (\link{FisherTest}) and must be one of "two.sided", "greater" or "less" (see also \link{fisher.test}).
#	method different clustering methods to compute. Character vector Availaible ones are: "ward", "flexible", "pam", "kmeans", "wards" (see \link{VegsoupDataPartition}) or missing which will compute all methods.
#	... arguments passed to VegsoupDataPartition (see \link{VegsoupDataPartition})

OptimStride <- function (obj, k, ft.treshold = 1e-3, alternative = "two.sided", method = c("ward", "flexible", "pam", "kmeans", "wards"), CALL = match.call(), ...) {
	if (missing(k)) stop("please supply k for stride")
	#	obj = dta; k = 3; alternative = "greater"
	cycle <- function (obj, k, ...) {
		prt <- VegsoupDataPartition(obj, k = k, ...)
		ft <- FisherTest(prt, alternative = alternative)
		res <- apply(ft < ft.treshold, 2, sum)
		return(res)
	}

	res.i <- vector("list", length = length(method))
	names(res.i) <- method
#	print(method)
	
	for (i in seq(along = method)) {
		print(method[i])
		res.j <- vector("list", length = k)
		names(res.j) <- 1:k
		res.j[[1]] <- 0
		names(res.j[[1]]) <- 1
		pb.j <- txtProgressBar(min = 2, max = k,
			char = '.', width = 45, style = 3)		
		for (j in 2:k) {
			setTxtProgressBar(pb.j, j)
			res.j[[j]] <- cycle(obj, method = method[i], k = j, ...)
		}
		res.i[[i]] <- res.j
		close(pb.j)
	}	
		
	#	develop class VegsoupDataOptimstride
	res <- new("VegsoupDataOptimstride", obj)
	
	optimstride <- list(
		indicators = res.i,
		settings = list(call = match.call(),
			args = c(as.list(match.call())[-c(1,2)])))
	optimstride$settings$args$method <- method
	optimstride$settings$args$ft.treshold <- ft.treshold
	optimstride$settings$args$alternative <- alternative

	res@optimstride <- optimstride
	return(res)
}

setMethod("show",
    signature(object = "VegsoupDataOptimstride"),
    function (object) {
			print(object@optimstride)
    }
)

.summaryVegsoupDataOptimstride <- function (object, oc.treshold = 2, ...) {

#	res.j <- rbind(c(0, 0), res.j)
#	dimnames(res.j) <- list(1:k, c("OptimClass1", "OptimClass2"))	

#	object <- foo
	obj <- object@optimstride
	args <- obj$settings$args
	met <- args$method
	ind <- obj$indicators
	ftt <- args$ft.treshold
	oct <- oc.treshold
	
	tmp <- sapply(ind, function (x) sapply(x, function (x) sum(x)))
			
	res <- list(optimclass1 = t(tmp), optimclass2 = apply(tmp, 2, function (x) sum(x >= oct)))
	cat("OptimStride results for k:", args$k)
	cat("\n\nOptimClass 1 (fisher test treshold: ", ftt, "):\n", sep = "")
	print(res$optimclass1)
	
	cat("
		\nOptimClass 2 (occurence treshold: ", oct, "):\n", sep = ""
	)
	print(res$optimclass2)

	return(invisible(res))
}

setMethod("summary",
    signature(object = "VegsoupDataOptimstride"),
	.summaryVegsoupDataOptimstride
)

#	print mode uses invisible()
#	use head(as.binray(obj))
setMethod("as.binary",
    signature(obj = "VegsoupData"),
    function (obj) {
			res <- obj@species != "0"
		mode(res) <- "numeric"
		res <- as.data.frame(res)
		return(invisible(res))
    }
)

plotOptimClass <- function (x, mode = c(1, 2)) {
	type <-  match.arg(as.character(mode), c("1", "2"))
	#	x = oc
	if (missing(mode)) mode = "1"
	k <- x$settings$k
	ft.treshold <- x$settings$ft.treshold
	oc.treshold <- x$settings$oc.treshold
	ylim.1 <- max(sapply(x$OptimClass, function(x) max(x[, 1])))
	ylim.2 <- max(sapply(x$OptimClass, function(x) max(x[, 2])))
	
	switch(mode,
		"1" = {
		plot(1:k, rep(max.oc, k),
			type = "n", ylim = c(0, ylim.1),
			xlab = "OptimClass1",
			ylab = "No. of faithfull species",
			sub = paste("Fisher's exact test, treshold",
			format(ft.treshold,scientific = TRUE)))
		for (i in seq(along = names(x$OptimClass))) {
			xx = 1:k
			yy = x$OptimClass[[i]][, 1]
			lines(xx, yy, lty = i)
		}
	}, "2" = {
		plot(1:k, rep(max.oc, k),
			type = "n", ylim = c(0, ylim.2),
			xlab = "OptimClass1",
			ylab = paste("No. of cluster with more than", oc.treshold, "faithful species"),
			sub = paste("Fisher's exact test, treshold",
			format(ft.treshold,scientific = TRUE)))
		for (i in seq(along = names(x$OptimClass))) {
			xx = 1:k
			yy = x$OptimClass[[i]][, 2]
			lines(xx, yy, lty = i)
		}	
	})
	legend("topright", lty = 1:length(method), legend = names(oc$OptimClass))
}

