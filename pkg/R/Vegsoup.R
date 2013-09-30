###	generating function
#	to do: improve documentation
Vegsoup <- function (x, y, z, coverscale, group, sp.points, sp.polygons, proj4string = "+init=epsg:4326", verbose = FALSE) {

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
	if (missing(y)) {
		stop("\nsites are missing!")	
	}
	else {
		if (class(y) == "Sites") {
			y <- sites(y) # formats numbers? 
		}
		else {
			y <- sites(new("Sites", data = y))
		}	
	}	
	if (missing(z)) {
			stop("\ntaxonomy is missing and x is not of class SpeciesTaxonomy!")
	}
	else {
		if (class(z) == "Taxonomy" | class(z) == "SpeciesTaxonomy") {
			z <- taxonomy(z)	
		}
		else {
			z <- taxonomy(new("Taxonomy", data = z))
		}			
	}
#	if (missing(decostand)) {
#		decostand = new("decostand", method = NULL)
#	} else {
#		decostand = new("decostand", method = decostand)
#	}	
#	if (missing(dist)) {
#		dist = "euclidean"
#	}	
	if	(!inherits(proj4string, "character")) {
		stop("\n... argument proj4string does not inhertit from class 'character'")
	}
		
	#	intersect x, y and z
	#	equal length
	xx <- sort(unique(x$plot))
	yy <- sort(unique(y$plot))
	test <- !isTRUE(identical(xx, yy))	
	
	if (test) {
		sel <- xx[xx == yy]
		x <- x[which(x$plot %in% sel), ]
		y <- y[which(y$plot %in% sel), ]
		z <- z[match(unique(x$abbr), z$abbr), ]
		warning("unique(x$plot) and unique(y$plot) do not match, ",
			"had to drop plots: \n",
			paste(xx[xx != yy], collapse = ", "), call. = FALSE)
	}
		
	if (missing(coverscale)) {
		if (verbose) {
			("\nno cover scale provided")
			xs <- Coverscale("braun.blanquet")
		}
		if (is.character(x$cov)) {
			message("interpret abundance values as character",
			"\nset cover scale to default 9 point Braun-Blanquet scale")
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

	#	test coverscale, must be valid	
	test <- any(is.na(factor(x$cov, # was !any
		levels = xs@codes,
		labels = xs@lims)))
	if (test) {
		stop("coverscale does not match data", call. = FALSE)
	}
	
	if (missing(group))	{
		group <- as.integer(rep(1, length(unique(x$plot))))
		names(group) <- unique(x$plot)
		if (verbose) {
			cat("\n no grouping factor supplied,",
				"use single partition")
		}
	}
	else {
		#	stopifnot(!is.null(names(group)))
		if (inherits(group, "numeric")) {
			group <- as.integer(group)
			names(group) <- unique(x$plot)
		}
		else {
			stop("argument group must be of mode integer", call. = FALSE)	
		}
	}
	
	if (missing(sp.points) & missing(sp.polygons))	{
		sp <- .find.coordinates(y, proj4string = proj4string)
		sp.points <- sp[[1]]
		sp.polygons <- sp[[2]]
	}

	#	drop coordiates from data frame	they are stored in spatial object
	y <- y[y$variable != "longitude" & y$variable != "latitude", ]
	if (any(sapply(y, is.factor))) {
		y <- as.data.frame(as.matrix(y),
			stringsAsFactors = FALSE)
	}

	#	rewrite!
	#	cast sites data	
	#	replace missing values
	if (any(y[, 3] == "") | any(is.na(y[, 3]))) {
		y[y[, 3] == "", 3] <- 0
		y[is.na(y[, 3]), 3] <- 0
		if (verbose) {
			message("\n NAs and empty fields (\"\") in supplied sites data",
			" filled with zeros")
		}
	}
   	
	y <- reshape(y[, 1:3],
		direction = "wide",
		timevar = "variable",
		idvar = "plot")   
	
	y[is.na(y)] <- 0 # needed? keep NAs?
	
	#	change column mode to numeric if possible
	
	#	y <- as.data.frame(sapply(y, type.convert, simplify = FALSE))
	options(warn = -1)
	y <- as.data.frame(
		sapply(y,
		function (x) {
			if (!any(is.na(as.numeric(x)))) {
				x <- as.numeric(x)
			}
			else {
				x <- as.character(x)	
			}
		}, simplify = FALSE),
		stringsAsFactors = FALSE)
	options(warn = 0)

 	#	groome names from reshape
 	names(y) <- gsub("value.", "", names(y), fixed = TRUE)
    #	assign row names
	rownames(y) <- y$plot
	y <- y[, -grep("plot", names(y))]
	#	order to x
	y <- y[match(unique(x$plot), rownames(y)), ]
	#	change longitude column!
	#	needs a work around because longitude is coreced to numeric
	#	because of similiarity to scientific notion (13.075533E)	
	sel <- grep("longitude", names(y))
	y[, sel] <- paste(as.character(y[, sel]), "E", sep = "")
	
	res <- new("Vegsoup",
		species = x,
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
