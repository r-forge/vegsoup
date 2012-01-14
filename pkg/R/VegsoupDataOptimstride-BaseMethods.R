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

#	OptimClass1: total count of faithful species across all clusters
#	OptimClass2: count of clusters that contain at least k faithful species, where k is a subjectively selected threshold value.
#	Tichy et al 2010: Journal of Vegetation Science 21: 287–299



setMethod("summary",
	signature(object = "VegsoupDataOptimstride"),
		function (object, oc.treshold = 2, silent = FALSE) {
			#	object <- os
		obj <- object@optimstride
		args <- obj$settings$args
		met <- args$method
		ind <- obj$indicators
		ftt <- args$ft.treshold
		oct <- oc.treshold
		args$oc.treshold <- oct
	
		oc1 <- t(sapply(ind, function (x) sapply(x, function (x) sum(x))))
		oc2 <- t(sapply(ind, function (x) sapply(x, function (x) length(which(x >= oct)))))
		
		res <- list(optimclass1 = oc1, optimclass2 = oc2, args = args)
	
		if (!silent) {
			cat("OptimStride results for k:", args$k)
			cat("\n\nOptimClass 1 (fisher test treshold: ", ftt, "):\n", sep = "")
			print(res$optimclass1)
	
			cat("\nOptimClass 2 (occurence treshold: ",
				oct, "):\n", sep = "")
			print(res$optimclass2)
		}
		return(invisible(res))
		}
)

if (!isGeneric("plot")) {
	setGeneric("plot", function(x, y, ...)
		standardGeneric("plot"))
}	

#	plot method
setMethod("plot",
	signature(x = "VegsoupDataOptimstride", y = "missing"),
	function (x, mode = 1, oc.treshold = 2, silent = TRUE, ...) {
	tmp <- summary(x, oc.treshold = oc.treshold, silent = silent)

	k <- tmp$args$k
	ft.treshold <- tmp$args$ft.treshold
	oc1 <- tmp$optimclass1
	oc2 <- tmp$optimclass2

	if (mode == 1) {
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc1)),
			xlab = "OptimClass1",
			ylab = "No. of significant indicator species",
			sub = paste("Fisher's exact test, treshold",
				format(ft.treshold, scientific = TRUE)), ...)
		for (i in 1:nrow(oc1)) {
			lines(1:k, oc1[i, ], lty = i)
		}
	}
	if (mode == 2) {
		plot(1:k, rep(0, k),
			type = "n", ylim = c(0, max(oc2)),
			xlab = "OptimClass2",
			ylab = paste("No. of cluster with more than",
				oc.treshold, "significant indicator species"),
			sub = paste("Fisher's exact test, treshold",
				format(ft.treshold,scientific = TRUE)), ...)
		for (i in 1:nrow(oc2)) {
			lines(1:k, oc2[i, ], lty = i)
		}
	}
	legend("topleft", lty = 1:length(tmp$args$method), legend = rownames(oc1), bty = "n")
}

)	