#	optparz defines
#	typal(clustering,dist,k=1)
#	typal samples in a partition
setGeneric("typical",
	function (obj, k = 1, ...)
		standardGeneric("typical")
)
".typical.Vegsoup" <- function (clustering, dist, k = 1) 
{
	require(optpart)
    clustering <- as.numeric(clustering)
	
    classes <- 1:length(table(clustering))
    part <- optpart::partana(clustering, dist)
    sil <- optpart::silhouette(clustering, dist)
    part.out <- matrix(NA, nrow = max(classes), ncol = k)
    for (i in classes) {
        tmp <- clustering == i
        names <- part$names[tmp]
        vals <- part$ptc[tmp, i]
        part.out[i, ] <- names[rev(order(vals))][1:k]
    }
    part.out <- data.frame(part.out)
    names(part.out) <- as.character(seq(1:k))
    sil.out <- matrix(NA, nrow = max(classes), ncol = k)
    for (i in classes) {
        tmp <- clustering == i
        names <- attr(dist, "Labels")[tmp]
        vals <- sil[tmp, 3]
        sil.out[i, ] <- names[rev(order(vals))][1:k]
    }
    sil.out <- data.frame(sil.out)
    names(sil.out) <- as.character(seq(1:k))
    out <- list(partana = part.out, silhouette = sil.out)
    out
}

setMethod("typical",
    signature(obj = "VegsoupPartition"),
    function (obj, k = 1, ...) {
    	
    	require(optpart)    	
    	cl <- match.call()    	
    	if (any(names(cl) == "mode")) {
    		if (cl$mode == "R") {
    			stop("\n method not defined for R mode", call. = FALSE)
    		}
    	}
		if (getK(obj) == 1) {
			warning(" results are meaningless with k = ",
				getK(obj), call. = FALSE)
			return(NULL)
		}
		else {
			d <- as.dist(obj, ...)
			p <- Partitioning(obj)
   			res <- optpart::typal(clustering = p, dist = d, k = k)
	   		return(res)
   		}
    }
)
