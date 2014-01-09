###	warning layers must be equal!!!
#	rbind like method to fuse objects
".bind.Vegsoup" <- function (..., deparse.level = 1) {

	allargs <- list(...)
	
	#	allargs <- list(eckkrammer2003tab1, eckkrammer2003tab2)
	
	#	test if all objects have the same abundance scale
	test <- length(unique((sapply(allargs,
			function (x) coverscale(x)@name))))
	if (test != 1) {
		stop("\n cover scale is not the same for all objects")
	}  else {
		#	fails!
		scale <- coverscale(allargs[[1]])
		#scale <- sapply(allargs, coverscale, simplify = FALSE)[[1]]
	}
	
	#	test if all objects have the same distance set
	#	if FALSE fall back to default eucliden
	dist <- "euclidean"
	#	test for overlapping plot ids
	tmp <- test <- unlist(sapply(allargs, rownames))
	test <- length(test) == length(unique(test))
	if (!test) {
		noquote(tmp[duplicated(tmp)])
		stop("\n there are overlapping plot names")
	}
	#	species 'x'	
	j <- vapply(allargs,
		FUN = function (x) nrow(Species(x)),
		FUN.VALUE = integer(1))
	#	todo: now use class Species
	#	implement rbind method	
    x <- as.data.frame(matrix("", nrow = sum(j), ncol = 4),
    	rownames = 1:sum(j), stringsAsFactors = FALSE)
    names(x) <- c("plot", "abbr", "layer", "cov")
    x$plot <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$plot))
    x$abbr <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$abbr))    
    x$layer <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$layer))        
    x$cov <- unlist(sapply(allargs,
    	FUN = function (x) Species(x)$cov))

	#	sites 'y'
	y <- do.call("rbind", sapply(allargs, .melt, simplify = FALSE))
	#	copied from Vegsoup.R!
	y <- reshape(y,	direction = "wide",
		timevar = "variable",
		idvar = "plot")
	#	groome names
	names(y) <- gsub("value.", "", names(y), fixed = TRUE)		
	y <- as.data.frame(sapply(y, type.convert, simplify = FALSE))
    #	assign row names
	rownames(y) <- y$plot 
	y <- y[, -grep("plot", names(y))]
	#	set NAs
	# y[is.na(y)] <- 0
	#	order y to x
	y <- y[match(unique(x$plot), rownames(y)), ]
	#	change longitude column!
	sel <- grep("longitude", names(y))
	y[, sel] <- paste(as.character(y[, sel]), "E", sep = "")
    #	taxonomy
	z <- do.call("rbind", sapply(allargs, Taxonomy, simplify = FALSE))
	z <- unique(z)
	z <- z[order(z$abbr), ]
	#	spatial points, taken from sp::rbind.SpatialPointsDataFrame because
	#	of dispatch issue
    pts <- sapply(allargs, SpatialPointsVegsoup, simplify = FALSE)
    names(pts) <- NULL
    sp <- do.call(sp::rbind.SpatialPoints, lapply(pts, function(x) as(x, "SpatialPoints")))
    df <- do.call(rbind, lapply(pts, function(x) x@data))
    pts <- sp::SpatialPointsDataFrame(sp, df, coords.nrs = pts[[1]]@coords.nrs)
	#	order pts to x
	pts <- pts[match(unique(x$plot), pts$plot), ] 
	#	stopifnot(all.equal(unique(x$plot), pts$plot))
	#	spatial polygons
	pgs <- sapply(allargs, SpatialPolygonsVegsoup, simplify = FALSE)
	names(pgs) <- NULL
    sp <- do.call(sp::rbind.SpatialPolygons, lapply(pgs, function(x) as(x, "SpatialPolygons")))
    df <- do.call(rbind, lapply(pgs, function(x) x@data))
    pgs <- sp::SpatialPolygonsDataFrame(sp, df, match.ID = FALSE)
	#	polygon IDs
	#ids <- unlist(sapply(allargs, function (x) { 
	#	sapply(slot(SpatialPolygonsVegsoup(x), "polygons"),
	#		function (x) slot(x, "ID")) # "Polygons"
	#}, simplify = FALSE))		
	#	order pgs to x
	pgs <- pgs[match(unique(x$plot), pgs$plot), ]
	
	res <- new("Vegsoup",
		species = x,
		sites = y, 
		taxonomy = z,
		coverscale = scale,
		dist = dist, 
		layers = as.character(unique(x$layer)),
		group = rep(integer(1), nrow(y)),
		sp.points = pts,
		sp.polygons = pgs
		)
	return(res)	    
}

#	Sites, Taxonomy Vegsoup have also rbind method
#if (!isGeneric("rbind")) {
setGeneric("bind",
		function (..., deparse.level = 1)
		standardGeneric("bind"),
		signature = "...")
#}

setMethod("bind",
    signature(... = "Vegsoup"),
	function (..., deparse.level = 1) { # add na.action argument
		.bind.Vegsoup(..., deparse.level = 1)	
	}
)