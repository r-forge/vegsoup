#	helper fucntion for Vegsoup()
#	random points in unit square
#	
.rpoisppSites <- function (x) {
	require(spatstat)
	require(maptools)
	n <- length(unique(x$plot))
	pts <- runifpoint(n, win = owin(c(0.2, 0.8), c(0.2, 0.8)) )
	pts <- as.SpatialPoints.ppp(pts)
	pts <- SpatialPointsDataFrame(pts,
		data = data.frame(plot = unique(x$plot),
			stringsAsFactors = FALSE))

	cents <- coordinates(pts)
	ids <- pts$plot

	pgs <- vector("list", nrow(cents))
	for (i in 1:nrow(cents)) {
		pg <- coordinates(GridTopology(cents[i,] - 0.05  /2, c(0.05, 0.05), c(2,2)))
		pg <- Polygons(list(Polygon(rbind(pg[c(1, 3 ,4 , 2),], pg[1, ]))), i)
		pgs[[i]] <- pg
	}

	pgs <- SpatialPolygons(pgs)
	pgs <- SpatialPolygonsDataFrame(pgs,
		data = data.frame(plot = unique(x$plot)))
	return(list(pts, pgs))
}

#	generating function
#	Objects can be created by calls of the form
#	Vegsoup(x, y, z, ...)
#	where x is a flat species data.frame
#	y is flat sites data.frame
#	and z is taxonomic reference list.
#	additional arguments include:
#	scale	abundance scale, possibly mixed?
#	group	grouping vector
#	sp.points	object of class SpatialPointsDataFrame
#		if missing random point pattern is generated
#	sp.polygons	object of class SpatialPolygonsDataFrame
#		if missing random polygon pattern is generated
#		based on sp.points

Vegsoup <- function (x, y, z, scale = c("Braun-Blanquet", "frequency", "binary"), group, sp.points, sp.polygons, verbose = TRUE) {
	#	x = species; y = sites; z = taxonomy; scale = list(scale = "binary")
	if (missing(x)) {
		x <- data.frame(NULL)
		stop("query on species is empty!\n")	
	} else {
		#	for safety
		x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)
		x  <- data.frame(x, stringsAsFactors = FALSE)[c("plot", "abbr", "layer", "cov")]			
		x <- x[order(x$plot, x$layer, x$abbr),]
	}

	if (missing(y)) {
		y <- data.frame(NULL)
		stop("query on sites is empty!\n")	
	} else {
		y <- as.data.frame(as.matrix(y), stringsAsFactors = FALSE)
	}	
	
	if (missing(z)) {
		z <- data.frame(NULL)		
		stop("query on taxonomy is empty!\n")
	} else {
		if (is.list(z) & any(names(z) == "species")) {
			z <- z$taxonomy
		}	
		z <- data.frame(z, stringsAsFactors = FALSE)[c("abbr", "taxon")]
		#	for safety
		z <- z[match(unique(x$abbr), z$abbr), ]
	}
	
	#	intersect species and sites

	if (length(unique(x$plot)) != length(unique(y$plot))) {
		warning("unique(x$plot) and unique(y$plot) do not match in length",
			" ... need to drop some data")
		sel <- intersect(unique(x$plot), unique(y$plot))
		x <- x[which(x$plot %in% sel), ]
		y <- y[which(y$plot %in% sel), ]		
	}
	
	#	subset z if necessary
	
		
	#	make valid names	
	x$abbr <- make.names(x$abbr)
	z$abbr <- make.names(z$abbr)
	
	if (missing(scale)) {
		warning("No cover scale provided", immediate. = TRUE)
		if (is.character(x$cov)) {
			warning("Interpret abundance values as charcter")
			warning("\nset cover scale to default 9 point Braun-Blanquet scale")
			scale <- list(
				scale = "Braun-Blanquet", 
				codes = c("r", "+", "1",
					"2m", "2a", "2b", "3", "4", "5"),
				lims = c(1, 2, 3, 4, 8, 18, 38, 68, 88))
			cat("\n", scale$codes)	
		} else {
			cat("cover seems to be numeric")
			cat("\nset abundance scale to frequency")
			scale <- c(scale = "frequency", list(codes = NULL), list(lims = NULL))
		}	
	} else {
		if (is.list(scale)) {
			if (length(scale) == 1)	{
				if (scale[[1]] == "frequency") {
					scale <- c(scale, list(codes = NULL), list(lims = NULL))
					x$cov <- as.numeric(x$cov)
					if (any(is.na(x$cov))) {
						str(x$cov)
						stop("there seems to be digits mixed with charcters?")
					}				
				#	stopifnot(is.numeric(x$cov))
				}
				if (scale[[1]] == "binary") {
					scale <- c(scale, list(codes = NULL), list(lims = NULL))
				stopifnot(dim(table(x$cov)) < 3)
				}
				if (scale[[1]] == "Braun-Blanquet") {
					scale <- list(
					scale = "Braun-Blanquet", 
					codes = c("r", "+", "1",
						"2m", "2a", "2b", "3", "4", "5"),
					lims = c(1, 2, 3, 4, 8, 18, 38, 68, 88))
				}
				if (scale[[1]] == "Braun-Blanquet 2") {
					scale <- list(
					scale = "Braun-Blanquet", 
					codes = c("r", "+", as.character(1:5)),
					lims = c(1, 2, 3, 13, 38, 68, 88))
				}
				if (scale[[1]] == "Domin") {
					scale <- list(
					scale = "Domin",
					codes <- c("+", as.character(1:9), "X"),
        			lims <- c(0, 0.01, 0.1, 1, 5, 10, 25, 33, 50, 75, 90, 100))
				
				}					
			}
		}	
	}

	if (missing(group))	{
		group <- as.integer(rep(1, length(unique(x$plot))))
		names(group) <- unique(x$plot)
		if (verbose) {
			cat("\nno grouping factor supplied,",
				"use single partition")
		}
	} else {
		stopifnot(!is.null(names(group)))
		if (!is.integer(group)) {
			group.names <- names(group)
			group <- as.integer(group)
			names(group) <- group.names	
		}
	}
	
	if (missing(sp.points) & missing(sp.polygons))	{
		#	generate random points
		
		#	try to find coordinates
		test <- any(y$variable == "longitude") & any(y$variable == "latitude")
		#	may raise errors in subset operations!
		if (verbose) {
			cat("\nattempt to retrieve coordinates from sites data ...")
		}

		if (test) {
			if (verbose) {		 
				cat("\n... found variables longitude and latitude!")
			}
			lng <- y[grep("longitude", y$variable), ]
			lat <- y[grep("latitude", y$variable), ]

			if (nrow(lng) == nrow(lat)) {
				lat <- lat[match(lng$plot, lat$plot), ]
				latlng <- data.frame(plot = lat$plot, latitude = lat$value, longitude = lng$value,
					stringsAsFactors = FALSE)
				latlng <- latlng[order(latlng$plot),]
					
				#	check decimal	
				latlng[,2] <- as.numeric(gsub(",", ".", latlng[,2], fixed = TRUE))
				latlng[,3] <- as.numeric(gsub(",", ".", latlng[,3], fixed = TRUE))
				sp.points <- latlng
				coordinates(sp.points) <- ~ longitude + latitude
				cat("")			
			} else {
				warning("\n... did not succeed. longitude and latitude do not match in length")
			}
			
			cents <- coordinates(sp.points)
			ids <- sp.points$plot

			pgs <- vector("list", nrow(cents))
			for (i in 1:nrow(cents)) {
			#	to do use plsx and plsy	
				pg <- coordinates(GridTopology(cents[i,] - 0.00005  /2, c(0.00005, 0.00005), c(2,2)))
				pg <- Polygons(list(Polygon(rbind(pg[c(1, 3 ,4 , 2),], pg[1, ]))), i)
				pgs[[i]] <- pg
			}
			#	length(SpatialPolygons(pgs))
			sp.polygons <- SpatialPolygonsDataFrame(SpatialPolygons(pgs),
					data = data.frame(plot = sp.points$plot))
					
			if (!all(sp.points$plot %in% x$plot)) {
				warning("\nnot a complete coordinates list, use random pattern")
				tmp <- .rpoisppSites(x)	
				sp.points <- tmp[[1]]
				sp.polygons <- tmp[[2]] 
			}
	
		} else {
			cat("\nSpatialPoints and SpatialPolygons missing, use random pattern")
			tmp <- .rpoisppSites(x)	
			sp.points <- tmp[[1]]
			sp.polygons <- tmp[[2]] 
		}	

	}
	
	if (any(sapply(y, is.factor))) {
		y <- as.data.frame(as.matrix(y),
			stringsAsFactors = FALSE)
	}
	
	#	order slot sites.long
	y <- y[order(y$plot, y$variable), ]
	
	res <- new("Vegsoup",
		species.long = x,
		sites.long = y,
		taxonomy = z,
		scale = as.list(scale),
		layers = as.character(unique(x$layer)),
		group = group,
		sp.points = sp.points,
		sp.polygons = sp.polygons
		)		
	res
}	

#	basic plotting function for testing
.plotVegsoup <- function (x, y, ...) {
	#	x = qry
	op <- par()
	on.exit(par(op))
	par(mfrow = c(2,2))
	plt <- x@species.long$plot		
	richness <- aggregate(rep(1, length(plt)),
		by = list(plt), sum)$x
	hist(richness, xlab = "Species richness", main = "")
    
	spc <- x@species.long$abbr
	occurences <- aggregate(rep(1, length(spc)),
		by = list(spc), sum)$x
    
	hist(occurences, xlab = "Species occurences", main = "")
	res <- list(richness = res.1, occurences = res.2)
	return(invisible(res))
}
    
setMethod("plot",
    signature(x = "Vegsoup", y = "missing"),
    .plotVegsoup
)

#	inherited methods

#	get or set layers

setGeneric("Layers",
	function (obj, ...)
	standardGeneric("Layers"))

setGeneric("Layers<-", function (obj, value)
	standardGeneric("Layers<-"))
		
setMethod("Layers",
    signature(obj = "Vegsoup"),
    function (obj) obj@layers
)

setReplaceMethod("Layers",
	signature(obj = "Vegsoup", value = "ANY"),
	function (obj, value) {
		obj@layers <- value
		obj@species.long$layer <- value
		#	to do: needs aggregation of cov
		return(obj)		
	}
)

#	method to return the layer columns from SpeciesLong(obj)
setGeneric("Layer",
	function (obj, ...)
		standardGeneric("Layer"))

.LayerVegsoupData <- function (obj) {
	SpeciesLong(obj)$layer
}

setMethod("Layer",
   signature(obj = "Vegsoup"),
    .LayerVegsoupData
)
	
#	get or set taxonomy (traits) data frame
setGeneric("Taxonomy",
	function (obj)
		standardGeneric("Taxonomy")
)
setGeneric("Taxonomy<-", function (obj, value)
	standardGeneric("Taxonomy<-"))
	
setMethod("Taxonomy",
    signature(obj = "Vegsoup"),
    function (obj) obj@taxonomy
)

setReplaceMethod("Taxonomy",
	signature(obj = "Vegsoup", value = "data.frame"),
	function (obj, value) {
		#	to do: needs checking against abbr
		obj@taxonomy <- value		
		return(obj)		
	}
)

#	get or set taxon abbreviation
setGeneric("Abbreviation",
	function (obj, ...)
		standardGeneric("Abbreviation")
)

setGeneric("Abbreviation<-",
	function (obj, value, ...)
		standardGeneric("Abbreviation<-")
)

setMethod("Abbreviation",
    signature(obj = "Vegsoup"),
    function (obj) sort(unique(obj@species.long$abbr))
)

setReplaceMethod("Abbreviation",
	signature(obj = "Vegsoup", value = "character"),
	function (obj, value) {
		#	to do: needs security for all slots!
		obj@species.long$abbr <- value		
		return(obj)		
	}
)

#	get or cover scale
setGeneric("AbundanceScale",
	function (obj)
		standardGeneric("AbundanceScale")
)

setGeneric("AbundanceScale<-",
	function (obj, value)
		standardGeneric("AbundanceScale<-")
)

setMethod("AbundanceScale",
    signature(obj = "Vegsoup"),
    function (obj) obj@scale
)

setReplaceMethod("AbundanceScale",
	signature(obj = "Vegsoup", value = "list"),
	function (obj, value) {
		#	to do: needs checking of list structure!
		#	to do: needs checking of species slots!
		obj@scale <- value
		#	obj@species
		#	obj@species.long
		return(obj)		
	}
)

#	get species query in long format
setGeneric("SpeciesLong",
	function (obj)
		standardGeneric("SpeciesLong")
)
setGeneric("SpeciesLong<-",
	function (obj, value)
		standardGeneric("SpeciesLong<-")
)
setMethod("SpeciesLong",
    signature(obj = "Vegsoup"),
    function (obj) obj@species.long
)
setReplaceMethod("SpeciesLong",
	signature(obj = "Vegsoup", value = "data.frame"),
	function (obj, value) {
		#	to do: needs checking of species slots!
		obj@species.long <- value
		#	obj@species
		#	obj@species.long
		return(obj)		
	}
)
#	get or set sites query in long format
setGeneric("SitesLong",
	function (obj)
		standardGeneric("SitesLong")
)
setGeneric("SitesLong<-",
	function (obj, value)
		standardGeneric("SitesLong<-")
)
setMethod("SitesLong",
    signature(obj = "Vegsoup"),
    function (obj) obj@sites.long
)
setReplaceMethod("SitesLong",
	signature(obj = "Vegsoup", value = "data.frame"),
	function (obj, value) {
		#	to do: needs checking of plot names
		obj@sites.long <- value
		return(obj)		
	}
)


#	get predefined grouping vector
setGeneric("AprioriGrouping",
	function (obj)
		standardGeneric("AprioriGrouping")
)
setMethod("AprioriGrouping",
    signature(obj = "Vegsoup"),
    function (obj) obj@group
)

#	get or set spatial points
#if (!isGeneric("getSpatialPoints"))
#if (!isGeneric("SpatialPoints"))
setGeneric("SpatialPointsVegsoup",
	function (obj)
		standardGeneric("SpatialPointsVegsoup")
)
#if (!isGeneric("SpatialPoints<-"))
setGeneric("SpatialPointsVegsoup<-",
	function (obj, value)
		standardGeneric("SpatialPointsVegsoup<-")
)
setMethod("SpatialPointsVegsoup",
    signature(obj = "Vegsoup"),
    function (obj) obj@sp.points
)
setReplaceMethod("SpatialPointsVegsoup",
	signature(obj = "Vegsoup", value = "SpatialPointsDataFrame"),
	function (obj, value) {
		#	to do: needs checking of plot names
		obj@sp.points <- value
		return(obj)		
	}
)
#	get spatial polygons
#if (!isGeneric("SpatialPolygons"))
setGeneric("SpatialPolygonsVegsoup",
	function (obj)
		standardGeneric("SpatialPolygonsVegsoup")
)
#if (!isGeneric("SpatialPolygons"))
setGeneric("SpatialPolygonsVegsoup<-",
	function (obj, value)
		standardGeneric("SpatialPolygonsVegsoup<-")
)
setMethod("SpatialPolygonsVegsoup",
    signature(obj = "Vegsoup"),
    function (obj) obj@sp.polygons
)
setReplaceMethod("SpatialPolygonsVegsoup",
	signature(obj = "Vegsoup", value = "SpatialPolygonsDataFrame"),
	function (obj, value) {
		#	to do: needs checking of plot names
		obj@sp.polygons <- value
		return(obj)		
	}
)

#	revert abunace scale for Braun-Blanquet scale
.BraunBlanquetReduce <-  function (obj) {

res <- SpeciesLong(obj)
for (i in c("2m", "2a", "2b")) {
	if (i == "2m")
		res$cov[res$cov == i]  <- "1"
	if (i == "2a")
		res$cov[res$cov == i]  <- "2"
	if (i == "2b")
		res$cov[res$cov == i]  <- "2"
}
	
obj@species.long <- res
obj@scale <- list(scale = "Braun-Blanquet 2",
	codes = c("r", "+", "1", "2", "3", "4", "5"),
	lims = c(1, 2, 3, 13, 38, 68, 88))
return(invisible(obj))
}

#if (!isGeneric("SpatialPolygons"))
setGeneric("BraunBlanquetReduce",
	function (obj, value)
		standardGeneric("BraunBlanquetReduce")
)

setMethod("BraunBlanquetReduce",
    signature(obj = "Vegsoup"),
    .BraunBlanquetReduce
)
	
#if (!isGeneric("Specieslist")) {
setGeneric("Specieslist",
	function (obj)
		standardGeneric("Specieslist")
)
#}
setMethod("Specieslist",
    signature(obj = "Vegsoup"),
    function (obj) {
    	res <- Taxonomy(obj)[c("abbr", "layer")]
	}
)