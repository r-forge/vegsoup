#	internal function to cast species matrix
.cast <- function (obj, mode, ...) {
	#	obj = dta; mode = 1
			
#	cpu.time <- system.time({
#	})
#	cat("\n time to init objects", cpu.time[3], "sec")
    		
	#	slots
	plot <- Species(obj)$plot
	abbr <- Species(obj)$abbr
	layer <- Species(obj)$layer	
	cov <- Species(obj)$cov
	scale <- coverscale(obj) # rename local object scale to ?
	
	#	matrix dimensions
	plots <- unique(plot)
	species.layer <- file.path(abbr, layer, fsep = "@") # faster than paste

	#	resort to layer
	if (length(Layers(obj)) > 1) {	
	#	rather slow, but ensures order
		species <- unique(as.vector(unlist(
			sapply(Layers(obj),
				function (x) species.layer[layer == x]
			))))
	} else {
	#	simple and faster if there is only one layer	
		species <- unique(species.layer)	
	}
			
	#	cover transformation
	if (mode == 1 & !is.null(scale@codes)) {
		cov <- as.numeric(as.character(
			factor(cov, levels = scale@codes, labels = scale@lims)
			))
		if (any(is.na(cov))) {
			stop("cover scale codes do not match data" )
		}	
	}
	if (mode == 1 & is.null(scale@codes)) {
		cov <- as.numeric(cov)		
	}

	if (mode == 1) {
		cpu.time <- system.time({	
		m <- t(vapply(plots,
			USE.NAMES = FALSE,
			FUN.VALUE = numeric(length(species)),
			FUN = function (x) {
				r <- numeric(length(species))
				r[match(species.layer[plot == x], species)] <- cov[plot == x]
				r
			}))
		dimnames(m) <- list(plots, species)		
		})
	}

	if (mode == 2) {
		cpu.time <- system.time({	
		m <- t(vapply(plots,
			USE.NAMES = FALSE,
			FUN.VALUE = character(length(species)),
			FUN = function (x) {
				r <- character(length(species))
				#	maybe change to "."
				#	there are several function that look for 0!
				r[] <- "0"
				r[match(species.layer[plot == x], species)] <- cov[plot == x]
				r
			}))
		dimnames(m) <- list(plots, species)		
		})
	}
		
	if (mode == 3) {
		cpu.time <- system.time({			
		m <- t(vapply(plots,
			USE.NAMES = FALSE,
			FUN.VALUE = integer(length(species)),
			FUN = function (x) {
				r <- integer(length(species))
				r[species %in% species.layer[plot == x]] <- as.integer(1)
				r
			}))
		dimnames(m) <- list(plots, species)
		})		
	}	
	
	return(invisible(m))
}

#	internal function to melt sites data.frame
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

#	helper function for VegsoupPartition

.VegsoupPartitionOptpartBestopt <- function (dist, k, numitr, verbose = TRUE) 
{
    if (class(dist) != "dist") {
        stop("bestopt is only defined for objects of class dist")
    }
    best <- 0
    ratios <- rep(0, numitr)
    for (i in 1:numitr) {
    	if (verbose) cat(".")
		tmp <- optpart(k, dist)        
        while (max(tmp$clustering) != k) {
        	tmp <- optpart(k, dist)
        	}
        ratios[i] <- max(tmp$ratio)
        if (ratios[i] > best) {
            best <- ratios[i]
            result <- tmp
            itr <- i
        }
    }
    cat("\nRatios for respective optparts \n")
    print(format(ratios, digits = 4))
    cat(paste("\nChoosing # ", itr, " ratio = ", format(best, 
        digits = 4), "\n"))
    invisible(result)
}

#	helper fucntion for .find.coordinates()
#	random points and polygons in unit square
#	
".rpoisppSites" <- function (x) {
	require(spatstat)
	require(maptools)
	n <- length(unique(x$plot)) # must be unique!
	pts <- runifpoint(n, win = owin(c(0.2, 0.8), c(0.2, 0.8)) )
	pts <- as.SpatialPoints.ppp(pts)
	pts <- SpatialPointsDataFrame(pts,
		data = data.frame(plot = sort(unique(x$plot)),
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
		data = data.frame(plot = sort(unique(x$plot))))
	return(list(pts, pgs))
}

#	try to find coordinates, otherwise generate random points

#	y <- Y@data
#	foo <- reshape(y[, 1:3],
#		direction = "wide",
#		timevar = "variable",
#		idvar = "plot")
	
".find.coordinates" <- function (y, proj4string, ...) {
	verbose = FALSE	
	lnglat.test <- any(y$variable == "longitude") & any(y$variable == "latitude")
	#	may raise errors in subset operations!
	if (verbose) {
		cat("\n attempt to retrieve coordinates from sites data ...\n")
	}
	
	#	coordiantes can be found
	if (lnglat.test) {
		if (verbose) {		 
			cat("\n found variables longitude and latitude!\n")
		}
		#	warning!
		#	check length of coordinates againts number of plots
		#	rn <- rownames(prt)
		#	pt <- prt@sp.points$plot
		#	rn[-match(pt, rn)]
		lng <- y[grep("longitude", y$variable), ]
		lat <- y[grep("latitude", y$variable), ]
		lnglat.test <- nrow(lng) == nrow(lat)
	
		#	spatial points			
		if (lnglat.test) {
			#	if coordinates variables are found
			#	select values and bind data.frame  
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
		
			#	test again if coordinates are really valid
			#	if not valid set flags
			if (!any(table(sp.points$plot) > 1)) {		
				coordinates(sp.points) <- ~ longitude + latitude
				lnglat.sim <- FALSE			
			} else {
				lnglat.test <- FALSE
				lnglat.sim <- TRUE			
				warning("did not succeed!",
					" Some coordinates seem to be doubled.",
					"\n problematic plots: ",
					paste(names(table(sp.points$plot)[table(sp.points$plot) > 1]),
					collapse = " "), call. = FALSE)
			}		
		} else {
			#	if no coordinates are found
			#	set falgs
			lnglat.test <- FALSE
			lnglat.sim <- TRUE			
			warning("did not succeed!",
				"\n longitude and latitude do not match in length", call. = FALSE)
		}

		#	spatial polygons from points, nested		
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
					data = data.frame(plot = as.character(ids),
						stringsAsFactors = FALSE))
			sp.polygons <- spChFIDs(sp.polygons, x = ids)				
		} else {		
			warning("not a complete coordinates list",
				" use random pattern instead", call. = FALSE)
			tmp <- .rpoisppSites(y)	
			sp.points <- tmp[[1]]
			sp.polygons <- tmp[[2]] 
		}	
	} else {
	#	or simulate	
		warning("spatialPoints and SpatialPolygons missing",
			" use random pattern", call. = FALSE)
			lnglat.sim <- TRUE
			tmp <- .rpoisppSites(y)	
			sp.points <- tmp[[1]]
			sp.polygons <- tmp[[2]]
	}
		
	if (!lnglat.sim) {
		proj4string(sp.points) <- CRS(proj4string)
		proj4string(sp.polygons) <- CRS(proj4string)	
	}
	return(list(sp.points, sp.polygons))		
}


#Average Bray-Curtis dissimilarity of an outlier plot to other plots is greater than two standard deviations from the mean inter-plot dissimilarity (McCune & Grace 2002)

#McCune, B. & Grace, J.B. 2002. Analysis of ecological communities. MjM Software design. Gleneden Beach OR, US.
#obj <- prt
#dis <- as.dist(obj)

#greater <- mean(dis) + sd(dis) * 2
#lower <- mean(dis) + sd(dis) * 2