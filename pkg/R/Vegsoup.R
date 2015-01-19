#	x <- XZ
#	y <- Y[Y$plot != "bs06", ]

#	generating function
Vegsoup <- function (x, y, z, coverscale, group, sp.points, sp.polygons, proj4string = "+init=epsg:4326", stringsAsFactors = TRUE, verbose = FALSE) {
	if (missing(x))
		stop("species are missing!")	
	if (missing(y))
		stop("sites are missing!")
	if (missing(z) & class(x) != "SpeciesTaxonomy")
		stop("taxonomy is missing and x is not of class SpeciesTaxonomy!")	
	if (!inherits(proj4string, "character"))
		stop("proj4string must be of class 'character'")

	#	if arguments are not of the desired class
	#	try to promote to class
	if (class(x) != "Species" & class(x) != "SpeciesTaxonomy")
		x <- new("Species", data = x)
	if (class(y) != "Sites")
		y <- new("Sites", data = y)						
	if (!missing(z)) if (class(z) != "Taxonomy")		
		z <- new("Taxonomy", data = z)	
	if (missing(z) & class(x) == "SpeciesTaxonomy")
		z <- taxonomy(x)		
	if (class(z) != "Taxonomy" & class(z) != "SpeciesTaxonomy")	
		z <- new("Taxonomy", data = z)
				
	#	intersect x, y (and z)
	if (!identical(x, y)) {
		i <- intersect(x, y)
		test <- sort(unique(c(x$plot, y$plot)))
		test <- test[!test %in% i]

		warning("identical(x, y) is FALSE, ",
			"had to drop ", length(test), " plot",
			ifelse(length(test) > 1, "s: ", " "),
			paste(test, collapse = ", "), call. = FALSE)
	
		x <- x[which(x$plot %in% i), ] # [-method for class SpeciesTaxonomy
		y <- y[which(y$plot %in% i), ]
		
		if (inherits(x, "SpeciesTaxonomy")) {
			# we have already subsetted the object and it's slots
			z <- taxonomy(x)
			x <- species(x)                        
		}
		else			
			z <- z[match(unique(x$abbr), z$abbr), ] # subset
	}
	
	#	intersect x and z
	if (!identical(x, z) & class(x) != "SpeciesTaxonomy") {
		i <- intersect(x, z)		
		z <- z[which(z$abbr %in% i), ]					
		z <- z[match(unique(x$abbr), z$abbr), ]
	}	
	
	#	all identical
	if (class(x) == "SpeciesTaxonomy") {
		z <- taxonomy(x)
		x <- species(x)		
	}
	
	stopifnot(identical(x, y))
	stopifnot(identical(x, z))	
				
	#	spatial
	if (missing(sp.points) & missing(sp.polygons))	{
		xy <- coordinates(y)
		d <- data.frame(plot = rownames(xy), row.names = rownames(xy),
			stringsAsFactors = FALSE)
		pt <- SpatialPointsDataFrame(xy, d, proj4string = CRS(proj4string))
		pg <- .polygonsSites(y, xy)
		
		#	drop coordiates from object
		y <- y[y$variable != "longitude" & y$variable != "latitude", ]
	}

	#	missing values, not very rigid!
	if (any(y$value == ""))
		y$value[y$value == ""] <- NA
	
	#	coerce to data.frame
	y <- as.data.frame(y)
	#	order to x
	y <- y[match(unique(x$plot), rownames(y)), ,drop = FALSE]	
	pt <- pt[match(rownames(y), pt$plot), ]
	pg <- pg[match(rownames(y), pg$plot), ]
		
	#	coverscale: the coverscale	
	if (missing(coverscale)) {
		if (verbose) {
			("\nno cover scale provided")
			xs <- Coverscale("braun.blanquet")
		}
		if (is.character(x$cov)) {
			if (verbose) {
				message("interpret abundance values as character",
				"\ntry to set cover scale to default 9 point Braun-Blanquet scale")
			}
			xs <- Coverscale("braun.blanquet")
		}
		else {
			message("cover seems to be numeric",
			"\nset abundance scale to percentage")
			xs <- Coverscale("percentage")
		}	
	}
	else {
		if (is.character(coverscale) & length(coverscale) == 1) {
			xs <- Coverscale(coverscale)
		}
		else { 
			if (inherits(coverscale, "Coverscale")) {
				xs <- coverscale
			}
			else {
				if (is.list(coverscale)) {
					#	problems with coerce methods will arise
					#	if setAs("list", "Coverscale") is defined
					#	currently not planned
					xs <- as(coverscale, "Coverscale")
				}
				else {
					stop("please supply a character, ",
						"list or object of class Coverscale", call. = FALSE)	
				}				
			}
		}
	}
	#	test if coverscale is ordinal
	if (is.ordinal(xs)) {
		test <- any(is.na(factor(x$cov,	xs@codes, xs@lims)))
		if (test) stop("coverscale does not match data", call. = FALSE)
	}
	#	test needed if continuous?
	if (is.continuous(xs)) {
	}		
	#	grouping
	if (missing(group))	{
		group <- as.integer(rep(1, length(unique(x$plot))))
		names(group) <- unique(x$plot)
		if (verbose) {
			message("\n no grouping factor supplied,",
				"use single partition")
		}
	}
	else {
		if (inherits(group, "numeric")) {
			group <- as.integer(group)
			names(group) <- unique(x$plot)
		}
		else {
			stop("argument group must be of mode integer", call. = FALSE)	
		}
	}
		
	r <- new("Vegsoup",
		species = x,
		sites = y, 
		taxonomy = z,
		coverscale = xs,
		layers = as.character(unique(x$layer)),
		decostand = new("decostand", method = NULL),
		dist = "euclidean",		
		group = group,
		sp.points = pt,
		sp.polygons = pg)
				
	return(r)
}	
