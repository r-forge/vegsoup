#	internal function to melt sites data frame
.melt <- function (obj) {
	require(stringr)
	#	obj = allargs[[10]]
	res <- data.frame(plot = rownames(slot(obj, "sites")),
				as.matrix(slot(obj, "sites")), stringsAsFactors = FALSE)
	res <- reshape(res, direction = "long",
		varying = names(res)[-1],
		v.names = "value",
		timevar = "variable",
		times = names(res)[-1],
		idvar = "plot", new.row.names = NULL)
	#	order by plot and create sequential rownames!	
	res <- res[order(res$plot), ]
	rownames(res) <- 1:nrow(res)
	#	width of numbers
	res$value <- str_trim(res$value, side = "left")
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
.rpoisppSites <- function (x) {
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
		pg <- coordinates(sp::GridTopology(cents[i,] - 0.05  /2, c(0.05, 0.05), c(2,2)))
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
	
.find.coordinates <- function (y, proj4string, ...) {
	
	verbose = FALSE
	
	lng <- grep("longitude", y$variable)
	lat <- grep("latitude", y$variable)
	
	lnglat.test <- length(lng) > 0 & length(lat) > 0
	#lnglat.test <- any(y$variable == "longitude") & any(y$variable == "latitude")
	#	may raise errors in subset operations!
	if (verbose) {
		message("attempt to retrieve coordinates")
	}
	
	#	coordiantes can be found
	if (lnglat.test) {
		if (verbose) {		 
			message("found variables longitude and latitude!")
		}
		#	warning!
		#	check length of coordinates againts number of plots
		#	rn <- rownames(prt)
		#	pt <- prt@sp.points$plot
		#	rn[-match(pt, rn)]
		lng <- y[lng, ]
		lat <- y[lat, ]
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
			#sp.points <- sp.points[order(sp.points$plot), ]
		
			#	test again if coordinates are really valid
			#	if not valid set flags
			if (!any(table(sp.points$plot) > 1)) {		
				coordinates(sp.points) <- ~ longitude + latitude
				lnglat.sim <- FALSE			
			} else {
				lnglat.test <- FALSE
				lnglat.sim <- TRUE			
				message("did not succeed!",
					" Some coordinates seem to be doubled.",
					"\n problematic plots: ",
					paste(names(table(sp.points$plot)[table(sp.points$plot) > 1]),
					collapse = " "))
			}		
		} else {
			#	if no coordinates are found
			#	set flags
			lnglat.test <- FALSE
			lnglat.sim <- TRUE			
			message("did not succeed!",
				"\n longitude and latitude do not match in length")
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
		}
		else {		
			message("not a complete coordinates list",
				" use random pattern instead")
			tmp <- .rpoisppSites(y)	
			sp.points <- tmp[[1]]
			sp.polygons <- tmp[[2]] 
		}	
	}
	else {
	#	if not simulate	
	#	message("use random pattern")
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

.texfile <- function (x, verbose, ...) {
	#	add file extension if missing
	if (length(grep(".tex", x, fixed = TRUE)) < 1) {
		x = paste0(x, ".tex")
	}
	#	replaced all blanks with underscores
	if (length(grep(" ", x, fixed = TRUE)) > 0) {
		x = gsub(" ", "_", x, fixed = TRUE)	
	}
	return(x)
}
