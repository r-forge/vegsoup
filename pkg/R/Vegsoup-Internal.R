#	internal function to melt sites data frame
.melt <- function (obj) {
	#	Suggests:
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
#	credits go to Dave Roberts
.VegsoupPartitionOptpartBestopt <- function (dist, k, numitr, verbose = TRUE) {
    if (class(dist) != "dist") {
        stop("bestopt is only defined for objects of class dist")
    }
    #	Imports: optpart
    #	require(optpart)
    
    best <- 0
    ratios <- rep(0, numitr)
    for (i in 1:numitr) {
    	if (verbose) cat(".")
		tmp <- optpart::optpart(k, dist)        
        while (max(tmp$clustering) != k) {
        	tmp <- optpart::optpart(k, dist)
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
	n <- length(unique(x$plot)) # must be unique!
	pts <- round(cbind(runif(n), runif(n)), 6)
	pts <- SpatialPointsDataFrame(pts,
		data = data.frame(plot = as.character(sort(unique(x$plot))), # leading zeros!
			stringsAsFactors = FALSE))

	cents <- coordinates(pts)
	ids <- as.character(pts$plot) # leading zeros!

	pgs <- vector("list", nrow(cents))
	for (i in 1:nrow(cents)) {
		pg <- coordinates(sp::GridTopology(cents[i,] - 0.05  /2, c(0.05, 0.05), c(2,2)))
		pg <- sp::Polygons(list(sp::Polygon(rbind(pg[c(1, 3 ,4 , 2),], pg[1, ]))), pts$plot[i]) # ID
		pgs[[i]] <- pg
	}

	pgs <- sp::SpatialPolygons(pgs)
	df <- data.frame(plot = pts$plot, row.names = pts$plot)
	pgs <- sp::SpatialPolygonsDataFrame(pgs, data = data.frame(plot = df))
	 ## as.character(sort(unique(x$plot)))))
	return(list(pts, pgs))
}

#	try to find coordinates, otherwise generate random points
#	working and reliable code, 	
.find.coordinates <- function (y, proj4string, ...) {
	#	Imports:
	#	rewuire(sp)
	
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
				pg <- coordinates(sp::GridTopology(
					cents[i,] - 0.00005 / 2, c(0.00005, 0.00005), c(2,2)))
				pg <- sp::Polygons(list(sp::Polygon(rbind(pg[c(1, 3 ,4 , 2), ], pg[1, ]))), i)
				pgs[[i]] <- pg
			}

			sp.polygons <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(pgs),
					data = data.frame(plot = as.character(ids),
						stringsAsFactors = FALSE))
			sp.polygons <- sp::spChFIDs(sp.polygons, x = ids)			
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
		proj4string(sp.points) <- sp::CRS(proj4string)
		proj4string(sp.polygons) <- sp::CRS(proj4string)	
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

#	a little bit of old materials

#N <- nrow(obj)						# number of plots
#n.i <- colSums(getBin(obj))			# species frequencies
#N_pi <- table(Partitioning(obj))	# number of plots in partition
#n_pi <- Contingency(obj)			# number of occurences in partition

#	notation follows Bruelheide (1995, 2000)
#	cited in Chytry et al 2002:80

#	N: number of plots in the data set
#	N_p: number of plots in partition
#	n: number of occurences in the data set
#	n_p: number of occurences in parttion 

#ObservedFreqencyTable <- function (N, N_p, n, n_p) { # f(o)_i
#	res <- matrix(c(
#		n_p,			# n_pi[i,j] for the the i species
#		N_p - n_p,
#		n - n_p,
#		N - N_p - n + n_p), 2, 2)	
#	res[is.nan(res)] <- 0			# f(o)_i
#   return(res)	
#}

#ExpectedFreqencyTable <- function (N, N_p, n, n_p) { # f(e)_i
#	res <- matrix(c(
#		n * N_p / N,
#		(N - n) * N_p / N,
#		n * (N - N_p) / N,
#		(N -n) * (N - N_p) / N), 2, 2)	
#	res[is.nan(res)] <- 0
#   return(res)
#}

#for (i in 1:ncol(obj)) { # loop over species
#	n <- n.i[i]						# n	
#   for (j in 1:length(N_pi)) {		# loop over partitions
#		N_p <- N_pi[j]				# N_p
#		n_p <- n_pi[i,j]    	
#    foi <- ObservedFreqencyTable(N, N_p, n, n_p)
#    fei <- ExpectedFreqencyTable(N, N_p, n, n_p)
#    res1 <- 2 * sum(foi * log(foi / fei), na.rm = T)
#    res2 <- g.statistic(fei, correction = "none")
#    }
#}	


#".sidak" <- function(vecP) {
#
# This function corrects a vector of probabilities for multiple testing
# using the Bonferroni (1935) and Sidak (1967) corrections.
#
# References: Bonferroni (1935), Sidak (1967), Wright (1992).
#
# Bonferroni, C. E. 1935. Il calcolo delle assicurazioni su gruppi di teste. 
# Pp. 13-60 in: Studi in onore del Professore Salvatore Ortu Carboni. Roma.
#
# Sidak, Z. 1967. Rectangular confidence regions for the means of multivariate 
# normal distributions. Journal of the American Statistical Association 62:626-633.
#
# Wright, S. P. 1992. Adjusted P-values for simultaneous inference. 
# Biometrics 48: 1005-1013. 
#
#                  Pierre Legendre, May 2007
#	k = length(vecP)
#	
#	vecPB = 0
#	vecPS = 0
#	
#	for(i in 1:k) {
#	   bonf = vecP[i]*k
#	   if(bonf > 1) bonf=1
#	   vecPB = c(vecPB, bonf)
#	   vecPS = c(vecPS, (1-(1-vecP[i])^k))
#	   }
#	#
#	return(list(OriginalP=vecP, BonfP=vecPB[-1], SidakP=vecPS[-1]))
#}
