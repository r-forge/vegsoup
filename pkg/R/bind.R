###	warning layers must be equal!!!
#	rbind like method to fuse objects
".bind.Vegsoup" <- function (..., deparse.level = 1) {

	allargs <- list(...)
		
	#	test if all objects have the same abundance scale
	test <- length(unique((sapply(allargs,
			function (x) coverscale(x)@name))))
	if (test != 1)
		stop("\n cover scale is not the same for all objects")
	else
		scale <- coverscale(allargs[[1]])
	
	#	test if all objects have the same distance set
	#	if FALSE fall back to default eucliden
	dist <- "euclidean"
	#	test for overlapping plot ids
	tmp <- test <- unlist(sapply(allargs, rownames))
	test <- length(test) == length(unique(test))
	if (!test) {
		error1 <- ("there are overlapping plot names")
		error2 <- message(paste(tmp[duplicated(tmp)], collapse = " "))
		stop(error1, error2)
	}

	#	species
	#	invokes	explicit ordering!
	x <- do.call("bind", sapply(allargs, species))

	#	sites
	y <- do.call("rbind", sapply(allargs, .melt, simplify = FALSE))
	y <- as.data.frame(sites(y))
	#	order y to x
	y <- y[match(unique(x$plot), rownames(y)), ]

	#	taxonomy
	z <- sapply(allargs, taxonomy)
	z <- do.call("bind", z)
	
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

#	we set the generic here,
#   classes "Species", "Sites", "Taxonomy" and "SpeciesTaxonomy"
#	then define methods for their classes
#if (!isGeneric("bind")) {
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

".bind.VegsoupPartiton" <- function (..., deparse.level = 1) {
	allargs <- list(...)
	n <- length(allargs)
	k <- sapply(allargs, getK)
	p <- sapply(allargs, partitioning, simplify = FALSE)
	d <- sapply(allargs, vegdist)
	s <- sapply(allargs, decostand)
	
	if (length(unique(d)) != 1)
		warning("differnt values for vegdist")
	#	vector to add to partition
	a <- cumsum(k)
	a <- c(0, a[-n])
	
	#	new partitioning vector with names to restore order
	p <- unlist(sapply(1:n, function (i) p[[i]] + a[i], simplify = FALSE))
	
	#	revert to Vegsoup and bind, this implies reordering!
	r <- do.call("bind", sapply(allargs, as, "Vegsoup"))
	
	#	reorder to partitioning vector
	r <- r[match(names(p), rownames(r)), ]
	
	r <- VegsoupPartition(r, method = "external", clustering = p)
	
	#	set vegdist and decostand to the most frequent
	r@dist <- names(sort(table(d), decreasing = TRUE))[1]
	r@decostand <- new("decostand", method = names(sort(table(s), decreasing = TRUE))[1])

	return(r)
}	

setMethod("bind",
	signature(... = "VegsoupPartition"),
	function (..., deparse.level = 1) { # add na.action argument
		.bind.VegsoupPartiton(..., deparse.level = 1)
	}
)	