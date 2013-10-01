#if (!isGeneric("outlier")) {
setGeneric("outlier",
	function (obj, ...)
		standardGeneric("outlier")
)
#}

setMethod("outlier",
    signature(obj = "Vegsoup"),
    function (obj, type = c("mccune", "wildi"), thresh = 0.2, ...) {
    	if (missing(type)) {
    		type <- "mccune"
    	}    	
    	if (!missing(thresh)) {
    		type <- "wildi"
    	}
    	else {
    		thresh = 0.2
    	} 
	    TYPE <- c("wildi", "mccune")            
	    type <- match.arg(type, TYPE, several.ok = TRUE)

		if (type == "mccune") {
			message("outlier type McCune")
			vegdist(obj) <- "bray"
			d <- as.matrix(as.dist(obj))		
			res <- apply(d, 1, mean) >= apply(d, 1, sd) * 2
		}	
		
		if (type == "wildi") {
			message("outlier type Wildi, theshold: ", thresh)
			#	adopted from function outly.R in package 'dave' by Otto Wildi
			#	see Wildi 2013 page ...			
			m <- as.matrix(obj)
			d <- as.matrix(as.dist((1 - cor(t(m ^ 0.5))) / 2)) 
			diag(d) <- 100
			nn <- apply(d, 1, min)				
			res <- nn >= thresh
			attr(res, "distances") <- nn
		}
	return(res)
	}
)