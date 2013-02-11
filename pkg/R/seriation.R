#	arrange an unpartitioned data set
setGeneric("seriation",
	function (obj, ...)
		standardGeneric("seriation")
)
setMethod("seriation",
    signature(obj = "Vegsoup"),
	function (obj, method, ...) {
	
	if (missing(method)) {
		method  <- "dca"
	} else {
		METHODS <- c("dca", "hclust", "ward", "flexible", "packed")
		method <- match.arg(method, METHODS)
	}
	
	si.dis <- as.dist(obj, "logical")
	sp.dis <- as.dist(obj, "logical", mode = "R")
	
	switch(method, dca = {
		use <- try(decorana(as.matrix(obj)), silent = TRUE, ...)
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
			si.ind <- order(rowSums(dta, "logical"), decreasing = TRUE)
			sp.ind  <- order(colSums(dta, "logical"), decreasing = TRUE)
		}
	)

	res <- obj[si.ind, sp.ind]
	return(res)

	}
)