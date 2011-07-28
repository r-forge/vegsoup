#	generating function
#	objects can be created by calls to Vegsoup(x, y, z)

VegsoupData <- function (obj, verbose = FALSE) {
	require(stats)
	#	obj <- qry; verbose = TRUE	
	if (!inherits(obj, "Vegsoup"))
		stop("Need object of class Vegsoup")

	scale <- AbundanceScale(obj)
	lay <- Layers(obj)
	txa <- Taxonomy(obj)
	species.long <- SpeciesLong(obj)
	
	if (scale$scale == "Braun-Blanquet") {
		stopifnot(is.character(species.long$cov))
		if (length(lay) == 1) {
			if (verbose) cat("\ndata is structered in only one layer")
		} else {
			if (verbose) cat("\ndata is structered in layers: ", lay)
		}
		
		cpu.time <- system.time({
		#	change coverscale to numeric using scale$lims
		tmp <- as.factor(species.long$cov)
		levels(tmp) <- scale$lims[match(levels(tmp), scale$codes)]
		species.long$cov <- as.numeric(as.character(tmp))

		xt  <- xtabs(cov ~ plot + abbr + layer, data = species.long)
		#	initialise species matrix	
		if (dim(xt)[3] > 1) {
			res <- matrix(0,
			ncol = dim(xt)[2] * dim(xt)[3],
			nrow = dim(xt)[1],
			dimnames = list(
				plot = dimnames(xt)$plot, 
				abbr = paste(rep(dimnames(xt)$abbr, dim(xt)[3]),
					rep(dimnames(xt)$layer,
					each = dim(xt)[2]), sep = "@")))
		} else {
			res <- matrix(0,
			ncol = dim(xt)[2],
			nrow = dim(xt)[1],
			dimnames = list(
				plot = dimnames(xt)$plot, 
				abbr = paste(dimnames(xt)$abbr,
						dimnames(xt)$layer, sep = "@")))
		}
		
		#	fill species matrix
		for (i in 1:dim(xt)[3]) {
			sel <- grep(paste("", dimnames(xt)$layer[i], sep = "@"),
				dimnames(res)$abbr, fixed = TRUE)
			res[,sel] <- xt[,,i]	
		}
		#	remove layers were species are absent
		res <- res[, colSums(res) > 0]
		res <- as.data.frame(res)
		#	restore coverscale to character using scale$codes
		for (i in seq(along = scale$lims)) {
			res[res == scale$lims[i]] <- scale$codes[i]
		}
		species <- res
		}) # end system.time
	} # end if "Braun Blanquet"
	
	cpu.time <- system.time({	
	if (scale$scale == "frequency") {

		if (!is.numeric(species.long$cov)) {
			warning("changed mode to numeric", str(species.long$cov))
			mode(species.long$cov) <- "numeric"
		}	
		xt  <- xtabs(cov ~ plot + abbr + layer,
			data = species.long)
	
		if (dim(xt)[3] > 1) {
			res <- matrix(0,
			ncol = dim(xt)[2] * dim(xt)[3],
			nrow = dim(xt)[1],
			dimnames = list(
				plot = dimnames(xt)$plot, 
				abbr = paste(rep(dimnames(xt)$abbr, dim(xt)[3]),
					rep(dimnames(xt)$layer,
					each = dim(xt)[2]), sep = "@")))
		} else {
			res <- matrix(0,
			ncol = dim(xt)[2],
			nrow = dim(xt)[1],
			dimnames = list(
				plot = dimnames(xt)$plot, 
				abbr = paste(dimnames(xt)$abbr,
						dimnames(xt)$layer, sep = "@")))
		}
		for (i in 1:dim(xt)[3]) {
			sel <- grep(paste("", dimnames(xt)$layer[i], sep = "@"),
				dimnames(res)$abbr, fixed = TRUE)
			res[,sel] <- xt[,,i]	
		}
		res <- res[,colSums(res) > 0]
		species <- as.data.frame(res)
	} # end if "frequency"	
	}) # end system.time
	
	if (verbose) {
		cat("\ntime to cast species matrix",
		"of", prod(dim(res)), "cells:",
		cpu.time[3], "sec\n")
	}
	#	cast sites data
	#	check missing values
	if (any(SitesLong(obj)[, 3] == "") | is.na(SitesLong(obj)[, 3]) ) {
		obj@sites.long[obj@sites.long[, 3] == "", 3] <- 0
		obj@sites.long[is.na(obj@sites.long[, 3]), 3] <- 0
		warning("NAs and empty fields (\"\") in supplied sites",
			" filled with zeros")
	}
	sites <- reshape(SitesLong(obj)[, 1:3],
		direction = "wide",
		timevar = "variable",
		idvar = "plot")
	#	tune data frame structure
	
	#	check NA's resulting from reshape
	#	missing variables values
	if (any(is.na(sites))) {
		sites[is.na(sites)] <- 0
		warning("NAs in casted sites data frame",
			" filled with zeros",
			"\nslot @sites.long will no longer match for these entries")
		#	to do
		#	paste back to @sites.long
	}
	
	#	change column mode to numeric if possible
	#	supress warning messages caused by as.numeric(x)
	options(warn = -1)
	sites <- as.data.frame(
		sapply(sites,
		function (x) {
			if (!any(is.na(as.numeric(x)))) {
				x <- as.numeric(x)
			} else {
				x <- as.character(x)	
			}
		}, simplify = FALSE),
		stringsAsFactors = FALSE)
	options(warn = 0)
	
	#	groome names
	names(sites) <- gsub("value.", "",
		names(sites), fixed = TRUE)
	rownames(sites) <- sites$plot
	sites <- sites[,-grep("plot", names(sites))]
	sites <- sites[match(rownames(species), rownames(sites)), ]
			
	res <- new("VegsoupData",
		species = species,
		sites = sites,
		taxonomy = obj@taxonomy,
		layers = obj@layers,
		species.long = obj@species.long,
		group = obj@group,
		sites.long = obj@sites.long,
		scale = obj@scale,
		sp.points = obj@sp.points,
		sp.polygons = obj@sp.polygons)

	return(res)
}

#	coercing methods
#	coercion to class Vegsoup is automatic as defined by the contains= argument	
setAs("VegsoupData", "list",
	def = function (from) {
		list(species = from@species,
		sites = from@sites)
	}
)

setMethod("names",
    signature(x = "VegsoupData"),
    function (x) names(x@species)
)

setMethod("rownames",
    signature(x = "VegsoupData", do.NULL = "missing",
    prefix = "missing"),
    function (x) rownames(x@species)
)

setMethod("dim",
    signature(x = "VegsoupData"),
	    function (x) dim(x@species)
)

setMethod("nrow",
    signature(x = "VegsoupData"),
    function (x) nrow(x@species)
)

setMethod("ncol",
    signature(x = "VegsoupData"),
    function (x) ncol(x@species)
)

setMethod("head",
    signature(x = "VegsoupData"),
    function (x, n = 6L, choice, ...) {
	    if (missing(choice))
	    	choice <- "species"	
    	if (choice == "species")
    		res <- head(x@species)
    	if (choice == "sites")
    		res <- head(x@sites)
    	return(res)
    }    	    
)

setMethod("tail",
    signature(x = "VegsoupData"),
    function (x, choice, ...) {
	    if (missing(choice))
	    	choice <- "species"
    	if (choice == "species")
    		res <- tail(x@species, ...)
    	if (choice == "sites")
    		res <- tail(x@sites, ...)
    	return(res)
    }    	    
)

setMethod("rowSums",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1) {
    	rowSums(as.binary(x))
    }
)

setMethod("colSums",
	signature(x = "VegsoupData"),
	function (x, na.rm = FALSE, dims = 1) {
    	colSums(as.binary(x))
    }
)
 
setMethod("coordinates",
   signature(obj = "VegsoupData"),
    function (obj) coordinates(obj@sp.points)
)

#	retrieve character matrix
setMethod("as.character",
    signature(x = "VegsoupData"),
    function (x) {
    	res <- as.matrix(x@species)
    	if (mode(res) != "character") {
	    	mode(res) <- "character"
    		res <- as.data.frame(res, stringsAsFactors = FALSE)
    	} else {
    		res <- x@species
    	}
    	return(invisible(res))
    }
)	

#	retrieve binary matrix
#if(!isGeneric("as.binary"))
setGeneric("as.binary",
	function (obj, ...)
		standardGeneric("as.binary")
)
#	print mode uses invisible()
#	use head(as.binray(obj))
setMethod("as.binary",
    signature(obj = "VegsoupData"),
    function (obj) {
		res <- obj@species != "0"
		mode(res) <- "numeric"
		res <- as.data.frame(res)
		return(invisible(res))
    }
)	

#	retrieve numeric matrix as defined in AbundanceScale(obj)
#	print mode uses invisible()
#	use head(as.binray(obj))
setMethod("as.numeric",
    signature(x = "VegsoupData"),
    function (x, verbose = FALSE) {
    	#	x = dta
		if (AbundanceScale(x)$scale == "frequency") {
			res <- x@species
			if (verbose) {
				cat("  species matrix is numeric, scale:",
					AbundanceScale(x)$scale)
			}
		} else {		
			res <- x@species
			scale <- AbundanceScale(x)		
			tmp <- as.factor(c(as.matrix(res)))
			levels(tmp) <- scale$lims[match(levels(tmp), scale$codes)]
			tmp <- as.numeric(as.character(tmp))
			tmp[is.na(tmp)] <- 0
			res[,] <- tmp
		}
		return(invisible(res))   	
    }
)

#	sites
setGeneric("Sites",
	function (obj, ...)
		standardGeneric("Sites")
)
setGeneric("Sites<-",
	function (obj, value, ...)
		standardGeneric("Sites<-")
)
setMethod("Sites",
    signature(obj = "VegsoupData"),
    function (obj) obj@sites
)
setReplaceMethod("Sites",
	signature(obj = "VegsoupData", value = "data.frame"),
	function (obj, value) {
		#	to do: needs validity check for several slots!
		obj@sites <- value		
		return(obj)		
	}
)

setMethod("$", "VegsoupData", 
	function(x, name) {
		if (!("sites" %in% slotNames(x)))
			stop("no $ method for object without slot sites")
		x@sites[[name]]
	}
)

#	aggregate: layer means, the different layers are combined assuming there independence (a species occuring in two layers with a cover of 50% will result in a overall cover of 75%. sum will sum up cover values of all layers. (see ?tv.veg)
.LayersVegsoupData <- function (obj, collapse, aggregate = c("layer", "mean", "min", "max", "sum"), dec = 0, verbose = FALSE) {
if (missing(collapse) & missing(aggregate)) {
	return(obj@layers)	
} else {
	if (length(obj@layers) < 2) {
		if (verbose) warning("has already only a single layer: ", obj@layers)
		return(obj)
	} else {
	#	check supplied arguments	
	if (missing(aggregate)) {
		aggregate <- "layer"
	} else {
		aggregate <- match.arg(aggregate)	
	}
	if (missing(collapse)) {
		if (verbose) cat("collapse to a single layer\n")
			collapse <- rep("0l", length(obj@layers))
	} else {
		if (length(collapse) > length(obj@layers))
			stop("length of collapse vector must match length(Layers(obj))")
	}

	#	obj = dta; verbose = FALSE; aggregate = "mean"; collapse = c("hl", "sl", "tl", "tl", "hl")
	
	if (inherits(obj, "VegsoupData")) 
		res <- as(dta, "Vegsoup") else res <- obj

	species <- SpeciesLong(res)
	scale <- AbundanceScale(res)

	collapse <- matrix(c(res@layers, collapse),
		ncol = 2, nrow = length(res@layers),
		byrow = FALSE,
		dimnames = list(NULL, c("original", "collapsed")))
	
	if (verbose) print(collapse)		

	species$layer <- factor(species$layer)
	levels(species$layer) <- collapse[match(levels(species$layer), collapse[, 1]), 2]
	species$layer <- as.character(species$layer)

	scale.is.character <- is.character(species$cov)
	
	#	convert original abundance scale to numeric
	if (scale.is.character) {
		species$cov <- as.factor(species$cov)
		levels(species$cov) <- scale$lims[match(levels(species$cov), scale$codes)]
		species$cov <- as.numeric(as.character(species$cov))
	}
	
	#	aggregate layers
	species  <- switch(aggregate, mean = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = mean)
	}, min = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = min)				
	}, max = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = max)
	}, sum = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = sum)
	}, layer = {
		aggregate(cov ~ plot + abbr + layer, data = species,
			FUN = function (x) {
				round((1 - prod(1 - x / max(scale$lims))) * max(scale$lims), dec)
				})		
	})
	
	species <- species[order(species$plot, species$layer, species$abbr), ]

	if (any(max(species$cov) > max(scale$lims))) {
		warning("reduced maximum aggregated abundance value to fit into limits: ",
			min(scale$lims)[1], " to ", max(scale$lims))
		species$cov[species$cov >  max(scale$lims)] <- max(scale$lims)
	}
	
	species$cov <- ceiling(species$cov)
	
	#	back convert to original abundance scale if it was character
	if (scale.is.character) {	
		species$cov <- as.factor(species$cov)
		levels(species$cov) <- scale$codes[match(levels(species$cov), scale$lims)]
		species$cov <- as.character(species$cov)
	}
	
	res@species.long <- species
	res@layers <- unique(collapse[, 2])
	res <- VegsoupData(res)
	return(invisible(res))

	}
	}
}

setMethod("Layers",
   signature(obj = "VegsoupData"),
    .LayersVegsoupData
)

#	Richness of data set
.getRichnessVegsoupData <-  function (obj, choice = c("dataset", "sample"), ...) {
	#	obj = dta
	if (missing(choice)) choice <- "dataset"
	choice <- choices[pmatch(choice, c("dataset", "sample"))]
	if (is.na(choice)) choice <- "dataset"
		switch(choice, "dataset" = {
		res <- length(unique(DecomposeNames(obj))$abbr)
		}, "sample" = {
		res <- as.binary(Layers(obj, aggregate = "layer", verbose = FALSE))
		res <- rowSums(res)
		})		
#	obj = dta

	return(res)
}

setGeneric("Richness",
	function (obj, ...)
		standardGeneric("Richness")
)
setMethod("Richness",
    signature(obj = "VegsoupData"),
    .getRichnessVegsoupData
)

#	connectedness of dissimilarities
setGeneric("getDistconnected",
	function (obj, ...)
		standardGeneric("getDistconnected")
)
setMethod("getDistconnected",
	signature(obj = "VegsoupData"),
	function (obj, dis = "bray", ...) {
		distconnected(vegdist(as.numeric(obj), dis), ...)
	}
)

.summaryVegsoupData  <- function (object, choice = c("all", "species", "sites"), ...) {
#	object = dta
	if (missing(choice)) choice <- "all"
		choices <- c("all", "species", "sites")
	choice <- choices[pmatch(choice, choices)]
	if (is.na(choice)) choice <- "all"
	cat("object of class", class(object), "\n")
	species.summary <- paste(
		Richness(object), " species",
		"\n", dim(object)[1], " sites (sample plots)",
		"\nlayers ", length(Layers(object)),
		" (", paste(Layers(object), collapse = ", "), ")",
		"\nscale ", AbundanceScale(object)$scale,
		ifelse(length(object@taxonomy) > 0,
			"\ntaxomomy lookup table supplied ",
			"... but has non matching taxa!"),
		sep = ""
	)

	switch(choice, "all" = {
		cat(species.summary)
		cat("\nsites ")	
		str(Sites(object), no.list = TRUE)
	}, "species" = {
		cat(species.summary)
	}, "sites" = {
		cat("\nsites ")
		str(Sites(object))		
	})
}

#	showMethods("plot", classes ="VegsoupData")

setMethod("summary",
    signature(object = "VegsoupData"),
	.summaryVegsoupData
)

.plotVegsoupData <- function (x, y, ...)
{	
	if (!inherits(x, "VegsoupData"))
		stop("\n need object of class VegsoupData")
	if (!prod(dim(x)) < 10^5)	
		cat("Let me calculate capscale first ...")
	#	x = species
	warning("not implemented yet")
}

#	dispatch not working
setMethod("plot",
	signature(x = "VegsoupData", y = "ANY"),
	.plotVegsoupData
)

#	generic subsetting method for all slots
#	VegsoupPartition has its own method!
setMethod("[",
    signature(x = "VegsoupData",
    i = "ANY", j = "ANY", drop = "missing"),
	function (x, i, j, ..., drop = TRUE)
    {
	    #	debug
	    #	x = dta; i = 1:20; j = 1:20
	    res <- x
	    if (missing(i)) i <- rep(TRUE, nrow(res))
	    if (missing(j)) j <- rep(TRUE, ncol(res))

		ii <- apply(as.binary(x)[i,j], 1, sum) > 0 # plots
		if (any(ii == FALSE)) {
			cat("\n removed empty sites:",
				rownames(x)[i][!ii])	
		}
		
		jj <- apply(as.binary(x)[i,j], 2, sum) > 0 # species
		if (any(jj == FALSE)) {
			tmp <- names(x)[j][!jj]
			cat("\n removed empty species!")
		}
		
		res@species <- as.character(x)[i,j][ii,jj]
		res@species.long <-
			res@species.long[res@species.long$plot %in%
				rownames(res), ]
		res@species.long <-
			res@species.long[paste(res@species.long$abbr,
				res@species.long$layer, sep = "@") %in%
				names(res), ]
		res@sites <-
			res@sites[match(rownames(res),
				rownames(res@sites)), ]
		if (any(sapply(res@sites, is.na))) stop("Error")
		#	prone to error if ordering really matters?
		#	maybe needs reordering?
		res@sites.long <-
			res@sites.long[res@sites.long$plot %in%
				rownames(res), ]

		if (length(res@group) != 0) {
		res@group <-
			res@group[names(res@group) %in%
				rownames(as.character(x)[i,j][ii,jj])]
		}
		#	Abbreviation relies on already subsetted taxonomy!
		abbr <- sapply(strsplit(names(res), "@", fixed = TRUE),
		   function (x) x[1])
 		#	taxonomy is subsetted!
		res@taxonomy <- res@taxonomy[res@taxonomy$abbr %in% abbr, ]
		res@sp.points <- res@sp.points[i,]
		res@sp.polygons <- res@sp.polygons[i,]
	    return(res)
    }
)

#	Function to rearrange object (species and sites data frames)
#	by various reordering methods as option.
#	Currently only presence/absencse data is used,
#	and default options of methods apply.
#	Currently there is no way to pass down arguments
#	to functions?
#	
.VegsoupDataArrange <- function (object, method = c("dca", "hclust", "ward", "flexible", "pam", "packed"), dist = "bray", ...) {
#	object = dta
if (!inherits(object, "VegsoupData"))
	stop("Need an object of class VegsoupData")

if (missing(method)) {
	method  <- "dca"
	} else {
	method <- match.arg(method)			
}

si.dis <- vegdist(as.binary(object), method = dist)
sp.dis <- vegdist(t(as.binary(object)), method = dist)
	
switch(method, dca = {
	use <- try(decorana(as.binary(object)), silent = TRUE, ...)
	if (inherits(use, "try-error"))
		use  <- NULL 

	if (is.list(use)) {	
		tmp <- scores(use, choices = 1, display = "sites")
		si.ind <- order(tmp)
		sp.ind <- try(order(scores(use, choices = 1, 
                  display = "species")))
		if (inherits(sp.ind, "try-error")) 
			sp.ind <- order(wascores(tmp, object))
	} else {
			si.ind <- 1:dim(as.binary(object))[1]
			sp.ind <- 1:dim(as.binary(object))[2]
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
	}, pam = {
		si.ind <- agnes(si.dis, diss = TRUE,
			method = "ward")$order
		sp.ind <- agnes(sp.dis, diss = TRUE,
			method = "ward")$order	
	}, packed = {
		si.ind <- order(apply(as.binary(object), 1, sum), decreasing = TRUE)
		sp.ind  <- order(apply(as.binary(object), 2, sum), decreasing = TRUE)
	}
)
	
res <- object[si.ind, sp.ind]

return(res)

}

#	arrange und unpartitioned data set
setGeneric("Arrange",
	function (object, ...)
		standardGeneric("Arrange")
)
setMethod("Arrange",
    signature(obj = "VegsoupData"),
    .VegsoupDataArrange
)

#	congruence between indicator and target species.
#	Halme's indicator power
			
setGeneric("Indpower",
	function (obj, ...)
		standardGeneric("Indpower")
)

setMethod("Indpower",
    signature(obj = "VegsoupData"),
    function (obj, ...) {
    	res <- indpower(as.binary(obj), ...)
    	diag(res)  <- NA
    	if (type == 0)
			res <- rowMeans(res, na.rm = TRUE)
		return(res)    	
    }
)


#	compositional indicator species analysis
setGeneric("Indspc",
	function (obj, ...)
		standardGeneric("Indspc")
)

setMethod("Indspc",
    signature(obj = "VegsoupData"),
    function (obj, method, ...) {
    	if (inherits(obj, "VegsoupDataPartition")) {
    		dis <- vegdist(as.binary(obj), obj@dist)
    	} else {
   			if (missing(method)) {    			
				dis <- vegdist(as.binary(obj), "bray")
    		} else {
    			dis <- vegdist(as.binary(obj), ...)
    		}    		
  	 	}
    	res <- indspc(as.binary(obj), dis = dis, ...)
		return(res)    	
    }
)

	
#	convert abbr to taxon names from species matrix slot(obj, "species")
.DecomposeNamesVegsoupData <- function (obj, verbose = FALSE) {
#	obj <- dta; type = "nospace"
if (verbose)
	cat("\n use Vegsoup standard pattern taxa coding, blanks are dots")

#	string deparse functions
#	depreciated!
#	VegsoupData now constructs abbreviated taxa names
#	using make.names and pastes layer seperated with '@' 

#	prefered method
	
#	deparse compound taxon abbreviation and layer string
#	seperator is '@'

abbr <- sapply(strsplit(names(obj), "@", fixed = TRUE),
		   function (x) x[1])
layer <- sapply(strsplit(names(obj), "@", fixed = TRUE),
		   function (x) x[2])

taxon <- Taxonomy(obj)$taxon[match(abbr, Taxonomy(obj)$abbr)]
res <- data.frame(abbr.layer = names(obj), abbr, layer, taxon, stringsAsFactors = FALSE)


if (all(is.na(res$layer))) {
	warning("unable to deparse layer string, consider setting type to nospace")
}
if (all(is.na(res$taxon))) {
	warning("unable to deparse taxon string, consider setting type to nospace")
}
return(invisible(res))
}

setGeneric("DecomposeNames",
	function (obj, ...)
		standardGeneric("DecomposeNames")
)
setMethod("DecomposeNames",
	signature(obj = "VegsoupData"),
	.DecomposeNamesVegsoupData
)

#	get Abbrviation
setMethod("Abbreviation",
    signature(obj = "VegsoupData"),
    function (obj, ...) DecomposeNames(dta)$abbr
)

#	create several clusterings along a vector
#	suitable for plotting
#	return a list

.strideVegsoupData <- function (obj, method, stride, fidelity.method, partition.method, mode, verbose = TRUE, alpha = 0.05, ...) {
	if (missing(fidelity.method))
		fidelity.method = "IndVal.g"
	if (missing(partition.method))
		partition.method = "flexible"
	if (missing(stride))
		stride = ceiling(ncol(obj) / 10)
	if (missing(mode))
		mode = 0
	if (verbose) {
		cat("compute", stride, "partitions for stride")
		cat("\nrun SigFidelity with mode", mode)
	}

res <- vector("list", length = stride)
names(res) <- 1:stride

if (verbose) {
	pb.stride <- txtProgressBar(min = 1, max = stride,
	char = '.', width = 45, style = 3)
}

cpu.time <- system.time({
for (i in 1:stride) {
	if (verbose) {
		setTxtProgressBar(pb.stride, i)
	}
	i.prt <- VegsoupDataPartition(obj, k = i, method = partition.method)
	i.fid <- Fidelity(i.prt, method = fidelity.method, verbose = FALSE)
	stat.fid <- apply(getStat(i.fid), 1, sum)#, 2)

	if (i > 1) {
		i.sig.fid <- SigFidelity(i.prt, verbose = FALSE)
		i.sig.fid <- length(which(i.sig.fid$stat < alpha))
	} else {
		i.sig.fid <- 0
	}
	res[[i]] <- list(stat = stat.fid, n.sig = i.sig.fid)
}


stat <- sapply(res, function (x) x[[1]])
n.sig <- sapply(res, function (x) x$n.sig)
diff.stat <- t(rbind(0, apply(stat, 1, diff)))
diff.stat <- apply(diff.stat, 2,
	function(x)	c(sum(x[x > 0]), sum(x[x < 0])))
})	
if (verbose) {
	close(pb.stride)
	cat("\n  computed stride in", cpu.time[3], "sec")
}
res <- list(
	stat = stat,
	n.sig = n.sig,
	diff.stat = diff.stat)
	
return(invisible(res))
}

setGeneric("Stride",
	function (obj, ...)
		standardGeneric("Stride")
)
setMethod("Stride",
	signature(obj = "VegsoupData"),
	.strideVegsoupData	
)

#	plotting method for stride list
plotStride <- function (x) {
#	x = Stride(dta)
	xx <- 1:(ncol(x$stat)) # getK(obj)
	y1 <- x$diff.stat[1, ]
	y2 <- x$diff.stat[2, ]
	ylim  <- range(x$diff.stat)
	
	r1 <- t(rbind(xright = xx - 0.5, ybottom = 0,
		xright = xx + 0.5, ytop = y1))
	r2 <- t(rbind(xright = xx - 0.5, ybottom = y2,
		xright = xx + 0.5, ytop = 0))
	bars <- cbind(xx, c(x$n.sig))[-1,] #?
	#	out.rug <- x@outgroups[,1]

	#	open plot
	par(mar= rep(4,4))
	plot(xx, y1, xlim = c(1, max(xx)),
		ylim = range(x$diff.stat), type = "n",
		xlab = "Cluster", ylab = "Index", frame.plot = FALSE) # getIndex(obj)
	#	sums of positive differences
	apply(r1, 1, function (x) rect(x[1], x[2], x[3], x[4],
		col = "grey50"))
	#	sums of negative differences
	apply(r2, 1, function (x) rect(x[1], x[2], x[3], x[4],
		col = "grey90"))
	#	number of significant indicator species
	plot.window(xlim = range(xx), ylim = range(bars[,2]))
	lines(xx[-1], bars[,2], type = "b",
		pch = 16, col = 2, lwd = 2, cex = 1)
	axis(4, col = 1, lwd = 1)	
	#	sum of all differnences
	plot.window(xlim = range(xx),
		ylim = range(apply(x$diff.stat, 2, sum)))
	lines(xx, apply(x$diff.stat, 2, sum),
		type = "b", pch = 15, cex = 1)
	
	#	mean significant indicator value	
#	plot.window(xlim = range(xx), ylim = range(apply(x$stat, 2, mean)))
#	lines(xx, apply(x$stat, 2, mean), type = "b", pch = 16, cex = 0.5)
		
	legend("top", lty = 1, col = c(1,2),
		legend = c("sum of all differences",
		"number of significant indicator species"),
		bty = "n")

#	if (any(out.rug > 0)) {
#		rug(out.rug[out.rug > 0], side = 3)
#		text(out.rug[out.rug > 0], par("usr")[4] * 1.1,
#			xpd = T, srt = 90)
#	}
}


#Average Bray-Curtis dissimilarity of an outlier plot to other plots is greater than two standard deviations from the mean inter-plot dissimilarity (McCune & Grace 2002)

#McCune, B. & Grace, J.B. 2002. Analysis of ecological communities. MjM Software design. Gleneden Beach OR, US.
#obj <- prt
#dis <- getDist(obj)

#greater <- mean(dis) + sd(dis) * 2
#lower <- mean(dis) + sd(dis) * 2