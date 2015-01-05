#	generating function
Vegsoup <- function (x, y, z, coverscale, group, sp.points, sp.polygons, proj4string = "+init=epsg:4326", stringsAsFactors = TRUE, verbose = FALSE) {
	#	Imports:
	#	require(sp) # otherwise GridTopology in .rpoisppSites() will fail?

	#	x: the species
	if (missing(x)) {
		stop("\nspecies are missing!")	
	}
	else {
		# get taxonomy from class SpeciesTaxonomy
		if (class(x) == "Species" | class(x) == "SpeciesTaxonomy") {
			if (!missing(z)) {
				x <- species(x)
			}
			else {
				z <- taxonomy(x)
				x <- species(x)				 
			}
		}
		else {
				x <- species(new("Species", data = x))
		}	
	}	
	#	y: the sites
	if (missing(y)) {
		stop("\nsites are missing!")	
	}
	else {
		if (class(y) == "Sites") {
			y <- sites(y)
		}
		else {
			y <- sites(new("Sites", data = y))
		}	
	}	
	#	z: the taxonomy	
	if (missing(z)) {
			stop("\ntaxonomy is missing and x is not of class SpeciesTaxonomy!")
	}
	else {
		if (class(z) == "Taxonomy" | class(z) == "SpeciesTaxonomy") {
			z <- taxonomy(z)
			zi <- FALSE	# no subset needed
		}
		else {
			z <- taxonomy(new("Taxonomy", data = z))
			zi <- TRUE # must subset
		}			
	}	
	if	(!inherits(proj4string, "character")) {
		stop("\nproj4string must inhertit from class 'character'")
	}		
	#	intersect x, y and z
	#	equal length
	xx <- sort(unique(x$plot))
	yy <- sort(unique(y$plot))
	test <- !isTRUE(identical(xx, yy))	
		
	if (test) {
		sel <- intersect(xx, yy) # was <- xx[xx == yy]
		x <- x[which(x$plot %in% sel), ]
		y <- y[which(y$plot %in% sel), ]
		z <- z[match(unique(x$abbr), z$abbr), ]
		
		test <- sort(unique(c(xx, yy)))
		test <- test[!test %in% sel]
		warning("unique(x$plot) and unique(y$plot) do not match, ",
			"had to drop ", length(test), " plots: ",
			paste(test, collapse = ", "), call. = FALSE)
	}	
	#	coverscale: the covercale	
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
		if (test) {
			stop("coverscale does not match data", call. = FALSE)
		}
	}
	#	test needed if continuous?
	if (is.continuous(xs)) {
	}		
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

	#	sites data		
	if (missing(sp.points) & missing(sp.polygons))	{
		sp <- .find.coordinates(y, proj4string = proj4string)
		sp.points <- sp[[1]]
		sp.polygons <- sp[[2]]
	}

	#	drop coordiates from data frame	they will be stored in spatial object
	y <- y[y$variable != "longitude" & y$variable != "latitude", , drop = FALSE]
	#	check missing values, not very rigid!
	if (any(y[, 3] == "")) {
		y[y[, 3] == "", 3] <- NA
		if (verbose) message("\n empty fields (\"\") in sites data set as NA")
	}  	
   	#	copied to bind.R!
	y <- reshape(y,	direction = "wide",
		timevar = "variable",
		idvar = "plot")
	names(y) <- gsub("value.", "", names(y), fixed = TRUE)
	#	save row names
	ii <- as.character(y$plot) # leading zeros!
	y <- y[, names(y) != "plot", drop = FALSE] 
	y <- as.data.frame(sapply(y,
		function (x) type.convert(x), simplify = FALSE))	
	if (!stringsAsFactors) {
		y <- as.data.frame(as.matrix(y),
			stringsAsFactors = FALSE)
	}	
    #	assign row names
	rownames(y) <- ii # leading zeros! 
	#	order to x
	y <- y[match(unique(x$plot), rownames(y)), ,drop = FALSE]	
	sp.points <- sp.points[match(rownames(y), sp.points$plot), ]
	sp.polygons <- sp.polygons[match(rownames(y), sp.polygons$plot), ]
	
	#	subset taxonomy if not supplied as class SpeciesTaxonomy
	if (zi)	z <- z[match(unique(x$abbr), z$abbr), ]
	
	res <- new("Vegsoup",
		species = species(x),
		sites = y, 
		taxonomy = z,
		coverscale = xs,
		layers = as.character(unique(x$layer)),
		decostand = new("decostand", method = NULL),
		dist = "euclidean",		
		group = group,
		sp.points = sp.points,
		sp.polygons = sp.polygons
		)		
	return(res)
}	
