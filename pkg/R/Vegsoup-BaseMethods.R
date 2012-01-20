###	generating function
#	to do: improve documentation, rewrite AbundanceScale interface
#	create a class AbundanceScale to handle more scales and allow user defined scales, high priority

Vegsoup <- function (x, y, z, scale = c("Braun-Blanquet", "Domin", "frequency", "binary"), group, sp.points, sp.polygons, proj4string = "+proj=longlat", verbose = TRUE) {
	#	x = species; y = sites; z = taxonomy$taxonomy; scale = list(scale = "Domin")
	if (missing(x)) {
		x <- data.frame(NULL)
		stop("query on species is empty!\n")	
	} else {
		#	for safety and to ensure validity
		x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)
		x  <- data.frame(x, stringsAsFactors = FALSE)[c("plot", "abbr", "layer", "cov")]
		
		if (any(regexpr("[[:alpha:]]", x$plot) < 1)) {
				warning("... plot identifier in species contains only numbers, ",
					"\nbut will be coded as character!", call. = FALSE)	
			x$plot <- as.character(as.numeric(species$plot))
		}	
		x <- x[order(x$plot, x$layer, x$abbr), ]
		if (dim(unique(unique(x)))[1] != dim(x)[1]) {
			warning("found duplicated species abundances for plots:\n... ",
				paste(x[duplicated(x), ]$plot, collapse = ", "),
				"\n apply unique(x, fromLast = FALSE) to get rid of duplicates in x!",
				"\n they will confuse me otherwise?",
				" please review your data!")
			x <- unique(x, fromLast = FALSE)
		} else {
			if (verbose) cat("\n species abundances for plots are unique, fine!" )
		}
		
	}

	if (missing(y)) {
		y <- data.frame(NULL)
		stop("query on sites is empty!\n")	
	} else {
		y <- as.data.frame(as.matrix(y), stringsAsFactors = FALSE)
				
		if (any(regexpr("[[:alpha:]]", y$plot) < 1)) {
				warning("... plot identifier in sites contains only numbers, ", 
					"\nbut will be coded as character!", call. = FALSE)	
			y$plot <- as.character(as.numeric(y$plot))
		}
		y <- y[order(y$plot, y$variable), ]
	}	
	
	if (missing(z)) {
		z <- data.frame(NULL)		
		stop("query on taxonomy is empty!\n")
	} else {
		if (is.list(z) & any(names(z) == "species")) {
			z <- z$taxonomy
		}	
		z <- data.frame(as.matrix(z), stringsAsFactors = FALSE)[c("abbr", "taxon")]
		#	for safety
		z <- z[match(unique(x$abbr), z$abbr), ]
	}
	
	if	(!inherits(proj4string, "character")) {
		stop("\n... argument proj4string does not inhertit from class 'character'")
	}
	#	make valid names	
	x$abbr <- make.names(x$abbr)
	z$abbr <- make.names(z$abbr)
	row.names(z) <- z$abbr
	
	#	intersect species and sites
	if (length(unique(x$plot)) != length(unique(y$plot))) {
		warning("... unique(x$plot) and unique(y$plot) do not match in length, ",
			"\nsome plots were dropped!", call. = FALSE)
		sel <- intersect(sort(unique(x$plot)), sort(unique(y$plot)))
		x <- x[which(x$plot %in% sel), ]
		y <- y[which(y$plot %in% sel), ]
		z <- z[match(unique(x$abbr), z$abbr), ]
	}
		
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
						codes = c("+", as.character(1:9), "X"),
        				lims = c(0.01, 0.1, 1, 5, 10, 25, 33, 50, 75, 90, 100))
				}					
			}
		} else {
			stop("please supply a list for argument scale")
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
		lnglat.test <- any(y$variable == "longitude") & any(y$variable == "latitude")
		#	may raise errors in subset operations!
		if (verbose) {
			cat("\nattempt to retrieve coordinates from sites data ...")
		}

		if (lnglat.test) {
			if (verbose) {		 
				cat("\n... found variables longitude and latitude!")
			}
			lng <- y[grep("longitude", y$variable), ]
			lat <- y[grep("latitude", y$variable), ]
			lnglat.test <- nrow(lng) == nrow(lat)
			
			if (lnglat.test) {
				lat <- lat[match(lng$plot, lat$plot), ]
				latlng <- data.frame(plot = lat$plot,
					latitude = lat$value, longitude = lng$value,
					stringsAsFactors = FALSE)
				latlng <- latlng[order(latlng$plot),]
				
				#	to do! implemnt char2dms
				
				#	strip of N and E
				latlng[, 2] <- gsub("[[:alpha:]]", "", latlng[,2])
				latlng[, 3] <- gsub("[[:alpha:]]", "", latlng[,3])
				
				#	strip of blanks
				latlng[, 2] <- gsub("[[:blank:]]", "", latlng[,2])
				latlng[, 3] <- gsub("[[:blank:]]", "", latlng[,3])
				
				#	check decimal and change mode
				latlng[, 2] <- as.numeric(gsub(",", ".", latlng[, 2], fixed = TRUE))
				latlng[, 3] <- as.numeric(gsub(",", ".", latlng[, 3], fixed = TRUE))
				
				sp.points <- latlng
				sp.points <- sp.points[order(sp.points$plot), ]

				if (!any(table(sp.points$plot) > 1)) {				
					coordinates(sp.points) <- ~ longitude + latitude
				} else {
					lnglat.test <- FALSE
					warning("... did not succeed!",
						" Some coordinates seem to be doubled.",
						"\nproblematic plots: ",
						paste(names(table(sp.points$plot)[table(sp.points$plot) > 1]), collapse = " "),
						call. = FALSE)
#					if (verbose) {
#						print(table(sp.points$plot)[table(sp.points$plot) > 1])
#					}	
				}		
			} else {
				lnglat.test <- FALSE
				warning("\n... did not succeed!",
					"\nlongitude and latitude do not match in length", call. = FALSE)
			}
			
			if (lnglat.test) {
				cents <- coordinates(sp.points)
				ids <- sp.points$plot
			
				#	plot polygons around centers
				#	to do! use plsx and plsy
				pgs <- vector("list", nrow(cents))
				for (i in 1:nrow(cents)) {
				#	to do use plsx and plsy	
					pg <- coordinates(GridTopology(cents[i,] - 0.00005  /2, c(0.00005, 0.00005), c(2,2)))
					pg <- Polygons(list(Polygon(rbind(pg[c(1, 3 ,4 , 2),], pg[1, ]))), i)
					pgs[[i]] <- pg
				}

				sp.polygons <- SpatialPolygonsDataFrame(SpatialPolygons(pgs),
						data = data.frame(plot = sp.points$plot))				
			} else {		
				warning("... not a complete coordinates list, use random pattern instead", call. = FALSE)
				tmp <- .rpoisppSites(x)	
				sp.points <- tmp[[1]]
				sp.polygons <- tmp[[2]] 
			}	
		} else {
			cat("\nSpatialPoints and SpatialPolygons missing, use random pattern")
			lnglat.sim <- TRUE
			tmp <- .rpoisppSites(x)	
			sp.points <- tmp[[1]]
			sp.polygons <- tmp[[2]]
		}	
	}
	
	if (!lnglat.sim) {
		proj4string(sp.points) <- CRS(proj4string)
		proj4string(sp.polygons) <- CRS(proj4string)	
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

#	summary method
#if (!isGeneric("summary")) {
setGeneric("summary", function(object, ...)
	standardGeneric("summary"))
#}
setMethod("summary",
    signature(object = "Vegsoup"),
	function (object) {
		cat("an object of class", class(object))
		if (is.null(Taxonomy(object))) {
			cat("\n  Taxonomy lookup table supplied")
		} else {
			cat("\n  Taxonomy lookup table missing")
		}
		cat("\n  proj4string:")
		print(proj4string(object))
		cat("\n  bbox:\n")
		print(bbox(object))
	}
)

#	inherited methods for show
setMethod("show",
    signature(object = "Vegsoup"),
    function (object) {
			summary(object)
    }
)


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
setMethod("Layer",
   signature(obj = "Vegsoup"),
	function (obj, ...) SpeciesLong(obj)$layer
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

### spatial methods

#	bbox method
#	warning: returns bbox only for SpatialPoints!
setMethod("bbox",
	signature(obj = "Vegsoup"),
    function (obj) bbox(obj@sp.points)
)

#	coordinates method
setMethod("coordinates",
	signature(obj = "Vegsoup"),
    function (obj) coordinates(obj@sp.points)
)

setReplaceMethod("coordinates",
	signature(object = "Vegsoup", value = "ANY"),
	function (object, value) 
		stop("setting coordinates cannot be done on Spatial objects, where they have already been set")
)
		
#	proj4string method
setMethod("proj4string",
	signature(obj = "Vegsoup"),
	function (obj) proj4string(obj@sp.points)
)

setReplaceMethod("proj4string",
	signature(obj = "Vegsoup", value = "character"),
	function (obj, value) {
		value <- CRS(value)
		proj4string(obj@sp.points) <- value
		obj
	}
)

setMethod("spTransform",
	signature(x = "Vegsoup", "CRS"),
	function (x, CRSobj, ...) {
		require(rgdal)
		x@sp.points <- spTransform(x@sp.points, CRSobj, ...)
		x@sp.polygons <- spTransform(x@sp.polygons, CRSobj, ...)
		x	
	}
	
)
#	get or set spatial points

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
	function (obj)
		standardGeneric("BraunBlanquetReduce")
)
setMethod("BraunBlanquetReduce",
    signature(obj = "Vegsoup"),
    .BraunBlanquetReduce
)
	
#if (!isGeneric("SpeciesList")) {
setGeneric("SpeciesList",
	function (obj, layered)
		standardGeneric("SpeciesList")
)
#}
setMethod("SpeciesList",
    signature(obj = "Vegsoup"),
    function (obj, layered = FALSE) {
    	if (missing(layered)) {
    		layered <- FALSE
    	}
    	if (layered) {
    	res <- SpeciesLong(obj)
    	res <- unique(res[c("abbr", "layer")])
    	res$taxon <- Taxonomy(dta)[res$abbr,]$taxon
    	res <- res[order(res$layer, res$taxon), ]			
    	} else {
    	res <- Taxonomy(obj)[c("abbr", "taxon")]	
    	}
    	return(invisible(res))	
	}
)

#	ploting method


#if (!isGeneric("plot")) {
setGeneric("plot", function(x, y, ...)
	standardGeneric("plot"))
#}	
  
setMethod("plot",
    signature(x = "Vegsoup", y = "missing"),
	function (x, ...) {

	opar <- par(mfrow = c(1,2))
	on.exit(par(opar))
	
	plt <- SpeciesLong(x)$plot		
	richness <- aggregate(rep(1, length(plt)),
		by = list(plt), sum)$x
	hist(richness, xlab = "Species richness", ...)
    
	spc <- x@species.long$abbr
	occurences <- aggregate(rep(1, length(spc)),
		by = list(spc), sum)$x
    
	hist(occurences, xlab = "Species occurences", ...)
	res <- list(richness, occurences)
	return(invisible(res))
}
)

#	inherited visulalisation method
#if (!isGeneric("gvisMap")) {
setGeneric("gvisMap",
	function (data, locationvar = "", tipvar = "", options = list(), chartid)
		standardGeneric("gvisMap")
)
#}

#	gvisMap package
setMethod("gvisMap",
    signature(data = "Vegsoup"),
    function (data) {
		require(gvisMap)
		pt <- SpatialPointsVegsoup(data)
		df <- data.frame(LatLong = apply(coordinates(pt)[, c(2,1)], 1, function (x) paste(x, collapse = ":")),
		Tip = pt$plot)
		m <- gvisMap(df, "LatLong" , "Tip")
		plot(m)
		return(invisible(m))
	}
)