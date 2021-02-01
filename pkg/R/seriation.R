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
	#	distance matrices
	Id <- as.dist(obj, "logical")
	#	as.dist lost argument mode = "R", generic is missing ... argument,
	#	as.matrix has a dots ... argument, we use this option!
	Jd <- vegan::vegdist(as.matrix(obj, "logical", mode = "R"),
		method = vegdist(obj))	
	#Jd <- as.dist(obj, "logical", mode = "R")	
	}
	switch(method, dca = {
		use <- try(decorana(obj), silent = TRUE, ...) # as.matrix dispatch
		if (inherits(use, "try-error")) {
			use <- NULL
		}	
		if (is.list(use)) {	
			tmp <- scores(use, choices = 1, display = "sites")
			i <- order(tmp)
			j <- try(order(scores(use, choices = 1, 
				  display = "species")))
			if (inherits(j, "try-error")) {
				j <- order(wascores(tmp, obj))
			}
		}
		else {
			i <- 1:dim(obj)[ 1 ]
			j <- 1:dim(obj)[ 2 ]
		}
		}, hclust = {
			i <- hclust(Id,
				method = "ward")$order
			j <- hclust(Jd,
				method = "ward")$order
		}, ward = {
			i <- agnes(Id, diss = TRUE,
				method = "ward")$order
			j <- agnes(Jd, diss = TRUE,
				method = "ward")$order
		}, flexible = {
		   	alpha <- 0.625
	   		beta = 1 - 2 * alpha
		   	i <- agnes(Id, method = "flexible",
		   		par.method = c(alpha, alpha, beta, 0))$order
	   		j <- agnes(Jd, method = "flexible",
	   			par.method = c(alpha, alpha, beta, 0))$order
		}, packed = {
			i <- order(rowSums(obj), decreasing = TRUE)
			j  <- order(colSums(obj), decreasing = TRUE)
		}
	)
	if (!missing(mode)) {
		if (mode == "R") {
			res <- obj[, j]	
		}
		if (mode == "Q") {
			res <- obj[i, ]
		}
	}
	else {	
		res <- obj[i, j]
	}
	
	return(res)
	
	}
)

setMethod("seriation",
	signature(obj = "VegsoupPartition"),
	function (obj, method, mode, ...) {
		res <- sapply(1:getK(obj), function (x) partition(obj, x))
		res <- sapply(res, function (x) seriation(as(x, "Vegsoup"), ...))		
		res <- obj[ unlist(sapply(res, rownames)), ]
		
		return(res)
	}
)
