#if (!isGeneric("heatmap")) {
setGeneric("outlier",
	function (obj, ...)
		standardGeneric("outlier")
)
#}


#.outlier <-
#function (obj, thresh = 0.2, y = 1, ...) {
#}

setMethod("outlier",
    signature(obj = "Vegsoup"),
    function (obj, thresh = 0.2, ...) {
	#	burrowed from outly.R in package 'dave' by Otto Wildi
	#	see Wildi 2013 page ., ...
	m <- as.matrix(obj)
	d <- as.matrix(as.dist((1 - cor(t(m ^ 0.5))) / 2)) 
	diag(d) <- 100
	res <- apply(d, 1, min) >= thresh
	#	print(names(res)[res])
	return(res)
    }
)


#outlier(hg)