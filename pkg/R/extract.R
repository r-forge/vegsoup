#	doubled indices should fail?!
setMethod("[",
    signature(x = "Vegsoup",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = TRUE) {
	    #	debug
	    #	x = dta; i = 1; j <- c(4,7,9,1,12); j <- rep(TRUE, ncol(x))
		#	x <- prt; i = Partitioning(x) == 2
	    
	    res <- x
	    
	    if (missing(i)) {
	    	i <- rep(TRUE, nrow(res))
	    }	
	    if (missing(j)) {
	    	j <- rep(TRUE, ncol(res))
	    }	
	    
	    #i[is.na(i)] <- FALSE	
	  	#j[is.na(j)] <- FALSE		    
	    
	    #	change to as.logical(x)[i, j, ...]
		#	when slot species is dropped
		tmp <- as.character(x)[i, j, drop = FALSE]
		#	validity
		if (all(unlist(tmp) == 0)) {
			stop("subset does not contain any species!", call. = FALSE)
		}
		#	if single plot "[" method will return class character
		if (class(tmp) == "character") {
			tmp <- t(matrix(tmp, dimnames = list(colnames(tmp), rownames(x)[i])))
		}	
		#	remove empty plots
		tmp <- tmp[rowSums(tmp != 0) > 0, , drop = FALSE]		
		#	remove empty species
		tmp <- tmp[, colSums(tmp != 0) > 0, drop = FALSE]
		#	assign slot species
		#	res@species <- as.data.frame(tmp, stringsAsFactors = FALSE)
        #	melt to long format
		cov <- array(t(tmp))
		plot <- rep(dimnames(tmp)[[1]], each = dim(tmp)[2])
		abbr.layer <- strsplit(
			rep(dimnames(tmp)[[2]], dim(tmp)[1]), "@", fixed = TRUE)
		abbr <- unlist(lapply(abbr.layer, "[[", 1))
		layer <- unlist(lapply(abbr.layer, "[[", 2))
		#	to do: use class 'species' here!
		res@species <- data.frame(plot, abbr, layer, cov,
			stringsAsFactors = FALSE)
       	res@species <- res@species[res@species$cov != 0, ]
       	#	new layer order
       	layer <- as.character(unique(res@species$layer))
       	layer <- layer[match(Layers(x), layer)]
       	layer <- layer[!is.na(layer)]
		#	subset sites
		res@sites <- res@sites[match(rownames(tmp),	rownames(Sites(res))), ]
		
		if (length(res@group) != 0) {
			res@group <- res@group[names(res@group) %in% rownames(tmp)]
		}
		#	method abbr relies on already subsetted taxonomy!
		abbr <- unlist(lapply(strsplit(colnames(res), "@", fixed = TRUE), "[[", 1))
 		#	finaly subset taxonomy, layers and spatial slots
		res@taxonomy <- res@taxonomy[res@taxonomy$abbr %in% abbr, ]
		res@layers <- layer 

		res@sp.points <- res@sp.points[match(rownames(tmp),
			res@sp.points$plot), ]
		res@sp.polygons <- res@sp.polygons[match(rownames(tmp),
			res@sp.polygons$plot), ]

	    return(res)
    }
)

setMethod("[",
    signature(x = "VegsoupPartition",
    i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE) {
		#	x <- prt; i = Partitioning(prt) %in% c(1,10)
	    part <- Partitioning(x)
	    
	    if (missing(i)) i <- rep(TRUE, nrow(x))
	    if (missing(j)) j <- rep(TRUE, ncol(x))
	    
	    tmp <- as(x, "Vegsoup")
	    tmp <- tmp[i, j, ...]
        
        if (FALSE) {  # a little bit too verbose         	
			if (length(unique(part[names(part) %in% rownames(tmp)])) != getK(x)) {
				message(" Partitioning vector was subsetted!",
					" k was changed accordingly")
			}
		}
		
		#	develop class VegsoupPartition from class Vegsoup
		res <- new("VegsoupPartition", tmp)
		#	and reassign class slots		
		res@part <- part[names(part) %in% rownames(tmp)]
		k <- length(unique(res@part))
		res@part[] <- as.integer(as.character(factor(res@part, labels = 1:k)))
		res@method = x@method
		res@k = k
		res@group = res@group[names(part) %in% rownames(tmp)]

		#	validity
		if (!identical(names(res@part), rownames(tmp))) {
			stop("inconsistency when subsetting partitioning vector")
		}
	    return(res)
    }
)

#setReplaceMethod("[", c("Vegsoup", "ANY", "missing", "ANY"), 
#	function(x, i, j, value) {
#		if (!("sites" %in% slotNames(x)))
#			stop("no [[ method for object without slot sites")
#		x@sites[[i]] <- value
#		x
#	}
#)

#	indexing method
setMethod("$", "Vegsoup", 
	function(x, name) {
		if (!("sites" %in% slotNames(x))) {
			stop("no $ method for object without slot sites")
		}
		return(x@sites[[name]])
		#do.call("$", list = (Sites(x), name))
	}
)

setReplaceMethod("$",
	signature(x = "Vegsoup"),
	function (x, name, value) {
		if (!("sites" %in% slotNames(x)))
			stop("no $<- method for object without attributes")		
 			x@sites[[name]] <- value 	
		return(x)		
	}
)