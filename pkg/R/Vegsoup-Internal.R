#	helper function for VegsoupDataPartition

.VegsoupDataPartitionOptpartBestopt <- function (dist, k, numitr, verbose = TRUE) 
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

#	helper fucntion for Vegsoup()
#	random points and polygons in unit square
#	
.rpoisppSites <- function (x) {
	require(spatstat)
	require(maptools)
	n <- length(unique(x$plot))
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
#Average Bray-Curtis dissimilarity of an outlier plot to other plots is greater than two standard deviations from the mean inter-plot dissimilarity (McCune & Grace 2002)

#McCune, B. & Grace, J.B. 2002. Analysis of ecological communities. MjM Software design. Gleneden Beach OR, US.
#obj <- prt
#dis <- as.dist(obj)

#greater <- mean(dis) + sd(dis) * 2
#lower <- mean(dis) + sd(dis) * 2