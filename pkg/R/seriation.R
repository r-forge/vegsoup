#	arrange an unpartitioned data set
setGeneric("seriation",
	function (obj, method, mode, ...)
		standardGeneric("seriation")
)
setMethod("seriation",
    signature(obj = "Vegsoup"),
	function (obj, method, mode, ...) {
	
	if (missing(method)) {
		method  <- "dca"
	}
	else {
		METHODS <- c("dca", "hclust", "ward", "flexible", "packed")
		method <- match.arg(method, METHODS)
	}
	if (!missing(mode)) {
		MODES <- c("R", "Q")
		mode <- match.arg(toupper(mode), MODES)
	}
	
	if (method != "dca" | method != "packed") {
	si.dis <- as.dist(obj, "logical")
	#	as.dist lost argument mode = "R", generic is missing
	#	... argument
	#	but, as.matrix has a dots ... argument!
	#	we use this
	sp.dis <- vegan::vegdist(as.matrix(obj, "logical", mode = "R"),
		method = vegdist(obj))	
	#sp.dis <- as.dist(obj, "logical", mode = "R")	
	}
	switch(method, dca = {
		use <- try(decorana(obj), silent = TRUE, ...) # as.matrix dispatch
		if (inherits(use, "try-error")) {
			use <- NULL
		}	
		if (is.list(use)) {	
			tmp <- scores(use, choices = 1, display = "sites")
			si.ind <- order(tmp)
			sp.ind <- try(order(scores(use, choices = 1, 
                  display = "species")))
			if (inherits(sp.ind, "try-error")) {
				sp.ind <- order(wascores(tmp, obj))
			}
		}
		else {
			si.ind <- 1:dim(obj)[1]
			sp.ind <- 1:dim(obj)[2]
		}
		}, hclust = {
			si.ind <- hclust(si.dis,
				method = "ward")$order
			sp.ind <- hclust(sp.dis,
				method = "ward")$order
		}, ward = {
			si.ind <- agnes(si.dis, diss = TRUE,
				method = "ward")$order
			sp.ind <- agnes(sp.dis, diss = TRUE,
				method = "ward")$order
		}, flexible = {
		   	alpha <- 0.625
	   		beta = 1 - 2 * alpha
		   	si.ind <- agnes(si.dis, method = "flexible",
		   		par.meth = c(alpha, alpha, beta, 0))$order
	   		sp.ind <- agnes(sp.dis, method = "flexible",
	   			par.meth = c(alpha, alpha, beta, 0))$order
		}, packed = {
			si.ind <- order(rowSums(obj), decreasing = TRUE)
			sp.ind  <- order(colSums(obj), decreasing = TRUE)
		}
	)
	if (!missing(mode)) {
		if (mode == "R") {
			res <- obj[, sp.ind]	
		}
		if (mode == "Q") {
			res <- obj[si.ind, ]
		}
	}
	else {	
		res <- obj[si.ind, sp.ind]
	}
	
	return(res)
	
	}
)

setMethod("seriation",
    signature(obj = "VegsoupPartition"),
	function (obj, method, mode, ...) {
		res <- lapply(1:getK(obj), function (x) obj[Partitioning(obj) == x, ])
		res <- sapply(res, function (x) seriation(as(x, "Vegsoup"), ...)) 
		res <- new("VegsoupPartition", do.call("rbind", res))
		res@k <- getK(obj)
		# this seems to be save based on lapply(1:getK(obj), ...)
		res@part <- sort(Partitioning(obj)) 		
		return(res)
	}
)
