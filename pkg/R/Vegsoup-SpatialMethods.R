### spatial methods

#	bbox method
#	warning: returns bbox only for SpatialPoints!
setMethod("bbox",
	signature(obj = "Vegsoup"),
	function (obj) bbox(obj@sp.points)
)

#	coordinates method for class Vegsoup*
setMethod("coordinates",
	signature(obj = "Vegsoup"),
	function (obj) coordinates(obj@sp.points)
)

#	taken from SpatialPointsDataFrame-methods in package sp
setReplaceMethod("coordinates",
	signature(object = "Vegsoup", value = "ANY"),
	function (object, value) {
		#	only formula method
		if (!inherits(value, "formula")) {
			stop("only formula method is implemented (e.g. coordinates(x) <- ~X+Y", call. = FALSE)
		}	
		xy <- model.frame(value, sites(object))
		if (dim(xy)[2] == 2) {
			jj <- as.character(as.list(value)[[2]])[2:3]
			j <- match(jj, names(object))
		} else if (dim(xy)[2] == 3) { 
			jj <- c(as.character(as.list((as.list(value)[[2]])[2])[[1]])[2:3],
				as.character(as.list(value)[[2]])[3])
			j <- match(j, names(object))
		}
		object@sp.points <- SpatialPointsDataFrame(
			coords = xy, data = slot(slot(object, "sp.points"), "data"),
			match.ID = FALSE)
		sites(object) <- sites(object)[, -j]
		return(object)
	}
)
		
#	proj4string method
setMethod("proj4string",
	signature(obj = "Vegsoup"),
	function (obj) proj4string(obj@sp.points)
)

#	hidden function to find coordinates, otherwise generate random points	
".coordinatesSites" <- function (obj) {
	#	objects of class Sites are ordered by plot and variable
	p <- unique(obj$plot)
	n <- length(p)
	#	we may get NAs, but, if at least one instance of the variable
	#	was found, we get a vector of length p, otherwise variable() returns NULL
	#	we can safely proceed with the following steps in this case
	x <- variable(obj, "longitude")
	y <- variable(obj, "latitude")

	#	strip of N, E and any blanks
	x <- gsub("[[:alpha:][:blank:]]", "", x)
	y <- gsub("[[:alpha:][:blank:]]", "", y)
	
	#	check decimal and change mode
	x <- as.numeric(gsub("[[:punct:]]", ".", x))
	y <- as.numeric(gsub("[[:punct:]]", ".", y))
	
	#	test success
	test0 <- !(length(x) == 0 | length(x) == 0) # if variables could not be found 
	test1 <- !any(is.na(x), is.na(y))		   # returns TRUE if test0 == TRUE
	test2 <- all(is.numeric(x), is.numeric(x))  # we must obtain numbers

	if (test0 & test1 & test2) {
		r <- cbind(x,y)
	} else {
		message("NAs introduced, use random pattern")		
		r <- cbind(x = runif(n), y = runif(n))
	}
	dimnames(r)[[1]] <- p
	return(r)
}	

#	coordinates method for class Sites
setMethod("coordinates",
	signature(obj = "Sites"),
	.coordinatesSites
)

#	hidden function to construct polygons around plot centers	
".polygonsSites" <- function (obj, x) {
	#	obj: Sites object
	#	x: matrix, as returned by coordiantes(Y)
	
	#	corner lengths of polygons
	a <- variable(obj, "plsx") # variable returns NULL is column is missing
	b <- variable(obj, "plsy")
		
	#	test if we got the variables and if they can be converted to numeric
	#	otherwise, apply default of 10 m
	ab <- rep(10, nrow(x))
	if ( is.null(a) | is.null(b) )	   a <- b <- ab
	if ( any(is.na(a)) | any(is.na(b)) ) a <- b <- ab
	if ( any(a == "") | any(b == "") )   a <- b <- ab
	#if (length(a) == 0 | length(a) == 0) a <- b <- ab	
	
	#	nasty decimals
	a <- gsub(",", ".", a)
	b <- gsub(",", ".", b)
	
	#	now we ensure numeric
	a <- as.numeric(a)
	b <- as.numeric(b)
	
	#	corner length in decimal degrees
	#	1 degree of latitude in meters is
	#	assuming short distances this should be sufficently accurate
	m <- (2 * pi * (6371) / 360) * 1000
	a <- a/m
	b <- b/m	
		
	n <- nrow(x)	
	ids <- rownames(x)
		
	r <- sapply(1:n, function (i) {
		#	corners of the polygon
		xi <- x[rep(i, 5), ]
		ai <- a[i]
		bi <- b[i]
		#	signs for a and b, clock wise
		sa <- c(-1,+1,+1,-1,-1)
		sb <- c(+1,+1,-1,-1,+1)
		#	
		xi[] <- xi + c((ai * sa), (bi * sb))
		
		sp::Polygons(list(sp::Polygon(xi)), ids[i])
	} )
	
	d <- data.frame(plot = ids, row.names = ids, stringsAsFactors = FALSE)
	r <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(r), data = d)
	return(r)
}



#setReplaceMethod("proj4string",
#	signature(obj = "Vegsoup", value = "character"),
#	function (obj, value) {
#		value <- CRS(value)
#		proj4string(obj@sp.points) <- value
#		obj
#	}
#)

setReplaceMethod("proj4string",
	signature(obj = "Vegsoup", value = "CRS"),
	function (obj, value) {
		proj4string(obj@sp.points) <- value
		proj4string(obj@sp.polygons) <- value
		obj
	}
)

setMethod("spTransform",
	signature(x = "Vegsoup", "CRS"),
	function (x, CRSobj, ...) {
		x@sp.points <- spTransform(x@sp.points, CRSobj, ...)
		x@sp.polygons <- spTransform(x@sp.polygons, CRSobj, ...)
		return(x)
	}
)
#	get spatial points
setGeneric("SpatialPointsVegsoup",
	function (obj)
		standardGeneric("SpatialPointsVegsoup")
)
setMethod("SpatialPointsVegsoup",
	signature(obj = "Vegsoup"),
	function (obj) {
		obj@sp.points
	}
)
#if (!isGeneric("SpatialPoints<-")) {
#setGeneric("SpatialPointsVegsoup<-",
#	function (obj, value)
#		standardGeneric("SpatialPointsVegsoup<-")
#)
#}
#setReplaceMethod("SpatialPointsVegsoup",
#	signature(obj = "Vegsoup", value = "SpatialPointsDataFrame"),
#	function (obj, value) {
#		#	to do: needs checking of plot names
#		obj@sp.points <- value
#		return(obj)		
#	}
#)

#	get spatial polygons
#if (!isGeneric("SpatialPolygons")) {
setGeneric("SpatialPolygonsVegsoup",
	function (obj)
		standardGeneric("SpatialPolygonsVegsoup")
)
#}
setMethod("SpatialPolygonsVegsoup",
	signature(obj = "Vegsoup"),
	function (obj) obj@sp.polygons
)
#if (!isGeneric("SpatialPolygons"))
#setGeneric("SpatialPolygonsVegsoup<-",
#	function (obj, value)
#		standardGeneric("SpatialPolygonsVegsoup<-")
#)

#setReplaceMethod("SpatialPolygonsVegsoup",
#	signature(obj = "Vegsoup", value = "SpatialPolygonsDataFrame"),
#	function (obj, value) {
#		#	to do: needs checking of plot names
#		obj@sp.polygons <- value
#		return(obj)		
#	}
#)

#	dispatch on Spatial*
setAs(from = "Vegsoup", to = "SpatialPoints",
	def = function (from) {
		as(from@sp.points, "SpatialPoints")
	}
)

#as.SpatialPoints.Vegsoup <- function (x) {
#	as(x, "SpatialPoints")
#}	

setAs(from = "Vegsoup", to = "SpatialPointsDataFrame",
	def = function (from) {
		from@sp.points
	}
)

#as.SpatialPointsDataFrame.Vegsoup <- function (x) {
#	as(x, "SpatialPointsDataFrame")
#}	

#	plot spatial points
points.Vegsoup <- function (x, y = NULL, ...) {
	points(coordinates(x), ...)
}