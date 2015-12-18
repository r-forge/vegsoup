setMethod("[",
	signature(x = "Vegsoup",
	i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE) {
		#	check arguments
		if (missing(i))
			i <- !logical(nrow(x)) # ?faster than rep(TRUE, nrow(x))
		else
			if (is.null(i)) i <- !logical(nrow(x))
			else
			if (any(is.na(i)))
				stop("NAs not allowed in indices", call. = FALSE)
			if (is.logical(i))
				stopifnot(length(i) == nrow(x))
	
		if (missing(j))
			j <- !logical(ncol(x))
		else
			if (is.null(j)) j <- !logical(ncol(x))
			else
			if (any(is.na(i)))
				stop("NAs not allowed in indices", call. = FALSE)
			if (is.logical(j))
				stopifnot(length(j) == ncol(x))
		
		#	rely on dimnames method
		ij <- dimnames(x)
		
		if (is.numeric(i) | is.logical(i))
			i <- ij[[1]][i]
		else
			if (is.character(i))
				i <- ij[[1]][ match(i, ij[[1]]) ]
			else
				stop("index must be one of numeric, logical or character")
		
		if (is.numeric(j) | is.logical(j))
			j <- ij[[2]][j]
		else
			if (is.character(j))
				j <- ij[[2]][ match(j, ij[[2]]) ]
			else
				stop("index must be one of numeric, logical or character")

		#	subsetted and/or permuted
		ij <- list(i, j)
		IJ <- dimnames(species(x))
		X <- species(x)
		
		#	subset but plots not permuted
		X <- X[(IJ[[1]] %in% ij[[1]] * IJ[[2]] %in% ij[[2]]) > 0, ]
		IJ <- dimnames(X)
		
		#	permute plots
		ii <- unlist(lapply(ij[[1]], function (i) which(i == IJ[[1]])))
		X <- X[ii, ]
			
		#!	don't permute species?
			
		if (nrow(X) < 1) stop("empty subset!", call. = FALSE)
			
		#	can not to be replaced with species<- because
		#	of a recursive call to "[" !
		x@species <- X

		#	subset remaining slots
		i <- unique(X$plot)
		
		###
		x@sites <- x@sites[match(i, rownames(sites(x))), ,drop = FALSE]
		x@taxonomy <- taxonomy(x)[taxonomy(x)$abbr %in% abbr(X), ]
		
		x@layers <- layers(x)[layers(x) %in% unique(X$layer)]
		if (length(x@group) != 0) x@group <- x@group[names(x@group) %in% i]

		x@sp.points <- x@sp.points[match(i, x@sp.points$plot), ]
		x@sp.polygons <- x@sp.polygons[match(i, x@sp.polygons$plot), ]

		return(x)
	}
)

setMethod("[",
	signature(x = "VegsoupPartition",
	i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE) {
		#	x <- prt; i = partitioning(prt) %in% c(1,10)
		part <- partitioning(x)
		
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
		r <- new("VegsoupPartition", tmp)
		#	and reassign class slots
		r@part <- part[match(rownames(r), names(part))]
		k <- length(unique(r@part))
		r@part[] <- as.integer(as.character(factor(r@part, labels = 1:k)))
		r@partitioning.method = x@partitioning.method
		r@k = k
		r@group = r@group[match(rownames(r), names(part))]

		#	validity
		if (!identical(names(r@part), rownames(tmp))) {
			stop("inconsistency when subsetting partitioning vector")
		}
		return(r)
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
		#do.call("$", list = (sites(x), name))
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