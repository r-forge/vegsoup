###	generating function
#	to do: improve documentation, rewrite AbundanceScale interface
#	create a class AbundanceScale to handle more scales and allow user defined scales, high priority

Vegsoup <- function (x, y, z, scale = c("Braun-Blanquet", "Braun-Blanquet 2", "Barkman", "frequency", "binary"), group, sp.points, sp.polygons, proj4string = "+init=epsg:4326", col.names = NULL, verbose = FALSE) {
	#	x = taxonomy$species; y = sites; z = taxonomy; scale = list(scale = "Braun-Blanquet")
	if (missing(col.names)) {
		col.names <- list(
			x = c("plot", "abbr", "layer", "cov"),
			y = c("plot", "variable", "value"),
			z = c("abbr", "taxon"))
	} else {	
		if (!is.list(col.names)) {
			stop("col.names must be a list")
		} else {
			if (length(col.names) != 3) {
				stop("col.names must be a list of character and length 3")
			} else {
				names(col.names) <- c("x", "y", "z")
				print(col.names)			
			}
		}
	}
	
	if (missing(x)) {
		x <- data.frame(NULL)
		stop("query on species is empty!\n")	
	} else {
		#	for safety and to ensure validity
		x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)[col.names$x]
		if (!ncol(x) == 4) {
			stop("unable to select colums: ",
				paste(col.names$x, collapse = " "),
				" from x")
		}
		names(x) <- c("plot", "abbr", "layer", "cov")
		
		if (any(regexpr("[[:alpha:]]", x$plot) < 1)) {
				warning("... plot identifier in x contains only numbers, ",
					"\nbut will be coded as character!", call. = FALSE)	
			x$plot <- as.character(as.numeric(species$plot))
		}	
		#	order
		x <- x[order(x$plot, x$layer, x$abbr), ]
		#	test for duplicated species
		#	robust test
		if (nrow(x[,c(1,2,3)]) != nrow(unique(x[,c(1,2,3)]))) {
			warning("\n found duplicated species for plots: ",
			"\n... ", paste(x[duplicated(x[, c(1,2,3)]), ]$plot, collapse = " "),
			"\n... ", paste(x[duplicated(x[, c(1,2,3)]), ]$abbr, collapse = " "),
			"\n drop all duplicates in x!",
			"\n they will confuse me otherwise?",
			"\n please review your data!", call. = FALSE)
			x <- x[!duplicated(x[, c(1,2,3)]), ]
		}
		#	further test
		if (dim(unique(unique(x)))[1] != dim(x)[1]) {
			warning("\n found duplicated species abundances for plots:\n... ",
				paste(x[duplicated(x), ]$plot, collapse = ", "),
				"\n apply unique(x, fromLast = FALSE) to get rid of duplicates in x!",
				"\n they will confuse me otherwise?",
				"\n please review your data!", call. = FALSE)
			x <- unique(x, fromLast = FALSE)
		} else {
			if (verbose) {
				cat("\n species abundances for plots are unique, fine!" )
			}
		}
	}

	if (missing(y)) {
		y <- data.frame(NULL)
		stop("query on sites is empty!\n")	
	} else {
		y <- as.data.frame(as.matrix(y), stringsAsFactors = FALSE)[col.names$y]
		stopifnot(ncol(y) == 3)
		names(y) = c("plot", "variable", "value")
				
		if (any(regexpr("[[:alpha:]]", y$plot) < 1)) {
				warning("\n ... plot identifier in sites contains only numbers, ", 
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
		z <- data.frame(as.matrix(z), stringsAsFactors = FALSE)[col.names$z]
		stopifnot(ncol(z) == 2)
		names(z) = c("abbr", "taxon")
		#	for safety
		z <- z[match(unique(x$abbr), z$abbr), ]
		#	alphabetic order
		z <- z[order(z$abbr), ]
	}
	
	if	(!inherits(proj4string, "character")) {
		stop("\n... argument proj4string does not inhertit from class 'character'")
	}
	
	#	make valid names	
	x$abbr <- make.names(x$abbr)
	z$abbr <- make.names(z$abbr)
	row.names(z) <- z$abbr
	
	#	intersect x, y and z
	if (length(unique(x$plot)) != length(unique(y$plot))) {
		sel <- intersect(sort(unique(x$plot)), sort(unique(y$plot)))
		x <- x[which(x$plot %in% sel), ]
		y <- y[which(y$plot %in% sel), ]
		z <- z[match(unique(x$abbr), z$abbr), ]
		warning("\n... unique(x$plot) and unique(y$plot) do not match in length, ",
			"\n some plots were dropped!", call. = FALSE)		
	}
		
	if (missing(scale)) {
		warning("\n no cover scale provided", call. = FALSE)
		if (is.character(x$cov)) {
			warning("\n interpret abundance values as character",
			"\n set cover scale to default 9 point Braun-Blanquet scale")
			scale <- list(
				scale = "Braun-Blanquet", 
				codes = c("r", "+", "1",
					"2m", "2a", "2b", "3", "4", "5"),
				lims = c(1, 2, 3, 4, 8, 18, 38, 68, 88))
			cat("\n", scale$codes)	
		} else {
			cat("\n cover seems to be numeric")
			cat("\n set abundance scale to frequency")
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
						stop("there seems to be digits mixed with characters?")
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
					stopifnot(!any(is.na(factor(x$cov,
						levels = scale$codes, labels = scale$lims))))
				}
				if (scale[[1]] == "Braun-Blanquet 2") {
					scale <- list(
						scale = "Braun-Blanquet 2", 
						codes = c("r", "+", as.character(1:5)),
						lims = c(1, 2, 3, 13, 38, 68, 88))
					stopifnot(!any(is.na(factor(x$cov,
						levels = scale$codes, labels = scale$lims))))
					print(scale)	
				}
				if (scale[[1]] == "Domin") {
					scale <- list(
						scale = "Domin",
						codes = c("+", as.character(1:9), "X"),
        				lims = c(0.01, 0.1, 1, 5, 10, 25, 33, 50, 75, 90, 100))
					stopifnot(!any(is.na(factor(x$cov,
						levels = scale$codes, labels = scale$lims))))
				}	
			}
		} else {
			stop("please supply a list for argument scale")
		}	
	}
	
	#	stoifnot(factor(cov, levels = scale$codes, labels = scale$lims))

	if (missing(group))	{
		group <- as.integer(rep(1, length(unique(x$plot))))
		names(group) <- unique(x$plot)
		if (verbose) {
			cat("\n no grouping factor supplied,",
				"use single partition")
		}
	} else {
		#	stopifnot(!is.null(names(group)))
		if (inherits(group, "numeric")) {
		#	group.names <- names(group)
			group <- as.integer(group)
			names(group) <- unique(x$plot)
		} else {
			stop("argument group must be of mode integer", call. = FALSE)	
		}
	}
	
	if (missing(sp.points) & missing(sp.polygons))	{
		#	try to find coordinates, otherwise generate random points
	
		lnglat.test <- any(y$variable == "longitude") & any(y$variable == "latitude")
		#	may raise errors in subset operations!
		if (verbose) {
			cat("\n attempt to retrieve coordinates from sites data ...\n")
		}

		if (lnglat.test) {
			if (verbose) {		 
				cat("\n found variables longitude and latitude!\n")
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
				latlng[, 2] <- gsub("[[:alpha:]]", "", latlng[, 2])
				latlng[, 3] <- gsub("[[:alpha:]]", "", latlng[, 3])				
				#	strip of blanks
				latlng[, 2] <- gsub("[[:blank:]]", "", latlng[, 2])
				latlng[, 3] <- gsub("[[:blank:]]", "", latlng[, 3])				
				#	check decimal and change mode
				latlng[, 2] <- as.numeric(gsub(",", ".", latlng[, 2], fixed = TRUE))
				latlng[, 3] <- as.numeric(gsub(",", ".", latlng[, 3], fixed = TRUE))
				
				sp.points <- latlng
				sp.points <- sp.points[order(sp.points$plot), ]

				if (!any(table(sp.points$plot) > 1)) {				
					coordinates(sp.points) <- ~ longitude + latitude
					lnglat.sim <- FALSE					
				} else {
					lnglat.test <- FALSE
					lnglat.sim <- TRUE					
					warning("\n did not succeed!",
						" Some coordinates seem to be doubled.",
						"\n problematic plots: ",
						paste(names(table(sp.points$plot)[table(sp.points$plot) > 1]),
							collapse = " "),
						call. = FALSE)
#					if (verbose) {
#						print(table(sp.points$plot)[table(sp.points$plot) > 1])
#					}	
				}		
			} else {
				lnglat.test <- FALSE
				lnglat.sim <- TRUE					
				warning("\n did not succeed!",
					"\n longitude and latitude do not match in length", call. = FALSE)
			}
			
			if (lnglat.test) {
				cents <- coordinates(sp.points)
				ids <- sp.points$plot
			
				#	plot polygons around centers
				#	to do! use plsx and plsy
				pgs <- vector("list", nrow(cents))
				for (i in 1:nrow(cents)) {
				#	to do use plsx and plsy	
					pg <- coordinates(GridTopology(
						cents[i,] - 0.00005 / 2, c(0.00005, 0.00005), c(2,2)))
					pg <- Polygons(list(Polygon(rbind(pg[c(1, 3 ,4 , 2), ], pg[1, ]))), i)
					pgs[[i]] <- pg
				}

				sp.polygons <- SpatialPolygonsDataFrame(SpatialPolygons(pgs),
						data = data.frame(plot = sp.points$plot))
				sp.polygons <- spChFIDs(sp.polygons, x = as.character(sp.polygons$plot))						
			} else {		
				warning("\n ... not a complete coordinates list",
					"use random pattern instead", call. = FALSE)
				tmp <- .rpoisppSites(x)	
				sp.points <- tmp[[1]]
				sp.polygons <- tmp[[2]] 
			}	
		} else {
			warning(paste("\n SpatialPoints and SpatialPolygons missing",
				"use random pattern"), call. = FALSE)
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
	
	#	order sites
	#y <- y[order(y$plot, y$variable), ]
	
	#	cast sites data	
	#	replace missing values
	if (any(y[, 3] == "") | any(is.na(y[, 3]))) {
		y[y[, 3] == "", 3] <- 0
		y[is.na(y[, 3]), 3] <- 0
		if (verbose) {
		cat("\n NAs and empty fields (\"\") in supplied sites data",
			" filled with zeros", call. = FALSE)
		}
	}
   	
	y <- reshape(y[, 1:3],
		direction = "wide",
		timevar = "variable",
		idvar = "plot")   
	
	y[is.na(y)] <- 0
	
	#	change column mode to numeric if possible
	#	supress warning messages caused by as.numeric(x) 
	#	needs a work around because longitude is coreced to numeric
	#	because of similiarity to scientifiuc notion (13.075533E)
	options(warn = -1)
	y <- as.data.frame(
		sapply(y,
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
 	names(y) <- gsub("value.", "", names(y), fixed = TRUE)
    #	assign row names
	rownames(y) <- y$plot
	y <- y[, -grep("plot", names(y))]
	#	order to x
	y <- y[match(unique(x$plot), rownames(y)), ]
	#	change longitude column!
	sel <- grep("longitude", names(y))
	y[, sel] <- paste(as.character(y[, sel]), "E", sep = "")
	
	res <- new("Vegsoup",
		species.long = x,
		sites = y, 
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
		if (is.null(Taxonomy(object)) || nrow(Taxonomy(object)) != length(Abbreviation(object))) {
			cat("\n taxonomy lookup table missing or uncomplete")
		} else {
			cat("\n taxonomy lookup table supplied and complete")
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
			do.call("summary", list(object))
    }
)

#	inherited methods for hist
#if (!isGeneric("hist")) {
setGeneric("hist",
	function (x, ...)
	standardGeneric("hist"))
#}	
#	ploting method hist
#setMethod("hist",
#	signature(obj = "Vegsoup"),
#	function (x, ...) {
#	   res <- 
#	   factor(x@species.long$cov,
#	   	levels = AbundancsScale(x)$codes,
#	   	lables = AbundancsScale(x)$lims)
#
#	   fig <- hist(x,
#	   main = "Histogram of abundance values",
#	   xlab = "mean abundance")
#	   fig
#	}
#)

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
		if (length(value) != length(Layers(obj))) {
			stop("length of value does not match length layers of object")
		}
		if (any(!Layers(obj) %in% value)) {
			stop("items of value do not match layers of object",
				"\n use Layers(obj, collapse = value),",
				" where layers to be dropped are coded as NA") 
		}
		obj@layers <- value
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
.melt <- function (obj) {
	#	obj = dta
	res <- stack(
			data.frame(plot = rownames(slot(obj, "sites")),
				slot(obj, "sites"), stringsAsFactors = FALSE),
		stringsAsFactors = FALSE) 

	plot <- res[res$ind == "plot", ]$values
	plot <- rep(plot, (nrow(res) / length(plot)) - 1)
	res <- res[!res$ind == "plot", ]
	res <- data.frame(
		plot = as.character(plot),
		variable = as.character(res[, 2]),
		value = as.character(res[, 1]),
		stringsAsFactors = FALSE)
	res <- res[order(res$plot), ]
	res[is.na(res)] <- ""

	rownames(res) <- 1:nrow(res)
	res	
}

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
    function (obj) .melt(obj)
)
setReplaceMethod("SitesLong",
	signature(obj = "Vegsoup", value = "data.frame"),
	function (obj, value) {
		#	to do: needs checking of plot names
		obj@sites <- value
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

#if (!isGeneric("SpatialPoints<-")) {
setGeneric("SpatialPointsVegsoup<-",
	function (obj, value)
		standardGeneric("SpatialPointsVegsoup<-")
)
#}
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
    		res$taxon <- Taxonomy(obj)[res$abbr, ]$taxon
	    	res <- res[order(res$layer, res$taxon), ]
	    	res <- res[, c("abbr", "taxon", "layer")]	    				
    	} else {
    		res <- Taxonomy(obj)[]	
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
setGeneric("QuickMap",
	function (obj)
		standardGeneric("QuickMap")
)
#}

#	gvisMap package
setMethod("QuickMap",
    signature(obj = "Vegsoup"),
    function (obj) {
		require(googleVis)
		pt <- SpatialPointsVegsoup(obj)
		if (nrow(pt) > 1) {
			df <- data.frame(LatLong = apply(coordinates(pt)[, c(2,1)], 1,
				function (x) paste(x, collapse = ":")),
			Tip = pt$plot)
		} else {
			df <- data.frame(LatLong = paste(coordinates(pt)[, c(2,1)], collapse = ":"),
			Tip = pt$plot)
		}
		m <- gvisMap(df, "LatLong" , "Tip")
		plot(m)
		return(invisible(m))
	}
)