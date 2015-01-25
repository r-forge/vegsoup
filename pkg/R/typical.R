#	optpart defines
#	typal(clustering,dist,k=1)
#	typal samples in a partition
setGeneric("typical",
	function (obj, k = 1, ...)
		standardGeneric("typical")
)

setMethod("typical",
	signature(obj = "VegsoupPartition"),
	function (obj, k = 1, ...) {
		
		cl <- match.call()
		if (any(names(cl) == "mode")) {
			if (cl$mode == "R") {
				stop("\n method not defined for R mode", call. = FALSE)
			}
		}
		if (getK(obj) == 1) {
			message("results are meaningless with k = ", getK(obj))
			return(NULL)
		}
		else {
			d <- as.dist(obj, ...)
			p <- partitioning(obj)
			res <- optpart::typal(clustering = p, dist = d, k = k)
			return(res)
		}
	}
)
