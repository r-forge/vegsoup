#	standardisation
#	vegan defines:
#	decostand(x, method, MARGIN, range.global, logbase = 2,
#	na.rm = FALSE, ...)

#if (!isGeneric("decostand")) {
setGeneric("decostand",
	function (x, method, MARGIN, range.global, logbase = 2, na.rm = FALSE, ...)
	standardGeneric("decostand"))
#}

#if (!isGeneric("decostand<-")) {
setGeneric("decostand<-",
	function (x, value)
		standardGeneric("decostand<-")
)
#}

setMethod("decostand",
		signature(x = "Vegsoup"),
			function (x) {
				slot(slot(x, "decostand"), "method")
			}
)

setReplaceMethod("decostand",
	signature(x = "Vegsoup", value = "character"),
	function (x, value) {
		#	taken from vegan
		METHODS <- c("total", "max", "frequency", "normalize", "range", 
			"standardize", "pa", "chi.square", "hellinger", "log",
			"wisconsin", "cap") # cap is not defined in vegan
		value <- match.arg(value, METHODS, several.ok = TRUE)
		x@decostand <- new("decostand", method = value)
		return(x)
	}
)

setReplaceMethod("decostand",
	signature(x = "VegsoupPartition", value = "character"),
	function (x, value) {
		#	taken from vegan
		METHODS <- c("total", "max", "frequency", "normalize", "range", 
			"standardize", "pa", "chi.square", "hellinger", "log",
			"wisconsin", "cap") # cap is defined in vegan
		value <- match.arg(value, METHODS, several.ok = TRUE)

		#	recompute partitioning
		if (is.null(decostand(x))) {
			x@decostand <- new("decostand", method = value)
				x <- VegsoupPartition(x, k = getK(x), method = x@partitioning.method)
				message("recomputed partitoning")
		} else {
			if (value != decostand(x)) {
				x@decostand <- new("decostand", method = value)
				x <- VegsoupPartition(x, k = getK(x), method = x@partitioning.method)
				message("recomputed partitoning")
			}
		}
		return(x)
	}
)

setReplaceMethod("decostand",
	signature(x = "Vegsoup", value = "NULL"),
 	function (x, value) {
		x@decostand <- new("decostand", method = NULL)
		return(x)
	}
)

setReplaceMethod("decostand",
	signature(x = "VegsoupPartition", value = "NULL"),
	function (x, value) {
		if (!is.null(decostand(x))) {	
			x@decostand <- new("decostand", method = NULL)
			#	recompute
			x <- VegsoupPartition(x, k = getK(x))
			message("recomputed partitoning")
		}
		return(x)
	}
)

#	cummulative abundance profile
#	De Caceres et al. 2013 Methods in Ecology and Evolution 4: 1167-1177
cap <- function (x, asVegsoup = FALSE) {
	a <- as.array(x, mode = "numeric") # default is 
	X <- species(species(x))
		
	#	reverse array, bring upper layer in front
	#	hl > sl > tl 
	a <- a[, , dim(a)[3]:1]
	#	we need the names
	n <- dimnames(a)
	
	#	apply cumsum over array
	#	we get values for lower layers if an upper one has a value
	#	we fix that by comparision with species object X
	r <- sapply(apply(a, 2:1, cumsum), unlist)

	#	cast to long format
	#	dim(a)[1] = plot, dim(a)[2] = abbr, dim(a)[3] = layer
	i <- rep(n$plot, each = dim(a)[2] * dim(a)[3])         # plots
	j <- rep(n$abbr, each = dim(a)[3], times = dim(a)[1])  # species
	z <- rep(n$layer, dim(a)[2] * dim(a)[1])               # layers
	r <- data.frame(plot = i, abbr = j, layer = z, cov = as.character(r))

	#	we must compare plot, abbr and layer selet the observations to retain
	xi <- sprintf("%s%s%s", X[, 1], X[, 2], X[, 3])
	ri <- sprintf("%s%s%s", r[, 1], r[, 2], r[, 3])
	
	r <- species(r[match(xi, ri), ])
	if (asVegsoup) {
		x@species <- r
		x@coverscale <- Coverscale("as.is") # must change
		return(x)
	}
	else
		return(r)
}