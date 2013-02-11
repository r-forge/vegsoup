#	subsetting method
#	to do: documenation
#	VegsoupPartition implemts its own method,
#	but internally coreces to class(Vegsoup)
#	and applies this method!
setMethod("[",
    signature(x = "Vegsoup",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = TRUE) {
	    #	debug
	    #	x = dta; i = 1; j <- c(4,7,9,1,12); j <- rep(TRUE, ncol(x))
		#	x <- prt; i = Partitioning(x) == 2
	    res <- x
	    if (missing(i)) i <- rep(TRUE, nrow(res))
	    if (missing(j)) j <- rep(TRUE, ncol(res))
	    #	change to as.logical(x)[i, j, ...]
		#	when slot species is dropped
		tmp <- as.character(x)[i, j, drop = FALSE]
		#	validity
		if (all(unlist(tmp) == 0)) {
			stop(call. = FALSE, "subset does not contain any species!")
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
		#	class 'species'				
		res@species <- data.frame(plot, abbr, layer, cov,
			stringsAsFactors = FALSE)
       	res@species <- res@species[res@species$cov != 0, ]
       	#	new layer order
       	layer <- as.character(unique(res@species$layer))
       	layer <- layer[match(Layers(x), layer)]
       	layer <- layer[!is.na(layer)]
		#	subset sites
		res@sites <- res@sites[match(rownames(tmp),	rownames(Sites(res))), ]
		if (any(sapply(res@sites, is.na))) {
			stop("NAs introduced in Sites(obj)")
		}	   
		if (length(res@group) != 0) {
			res@group <- res@group[names(res@group) %in% rownames(tmp)]
		}
		#	method Abbreviation relies on already subsetted taxonomy!
		abbr <- unlist(lapply(strsplit(colnames(res), "@", fixed = TRUE), "[[", 1))
 		#	finaly subset taxonomy, layers and spatial slots
		res@taxonomy <- res@taxonomy[res@taxonomy$abbr %in% abbr, ]
		res@layers <- layer 
		res@sp.points <- res@sp.points[match(rownames(tmp),
			SpatialPolygonsVegsoup(res)$plot), ]
		res@sp.polygons <- res@sp.polygons[match(rownames(tmp),
			SpatialPolygonsVegsoup(res)$plot), ]

	    return(res)
    }
)

setReplaceMethod("[", c("Vegsoup", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if (!("sites" %in% slotNames(x)))
			stop("no [[ method for object without slot sites")
		x@sites[[i]] <- value
		x
	}
)

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