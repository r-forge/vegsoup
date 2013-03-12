#	ellipsoidellipsoidhull around partitions
".ellipsoidhull" <- function (x, ...) {
#	x <- prt
cl <- match.call()

res <- vector("list", length = getK(x))
for (i in 1:getK(x)) {
	xy <- as.matrix(coordinates(x)[Partitioning(x) == i,])
	if (nrow(xy) >= 3) {
		res[[i]] <- cluster::ellipsoidhull(xy)
	} else {
		if (ncol(xy) > 1) {
			res[[i]] <- xy.coords(x = xy[, 1], y = xy[, 2])
		} else {
			res[[i]] <- xy.coords(x = t(xy)[, 1], y = t(xy)[, 2])		
		}
	}
}

points(SpatialPointsVegsoup(x))

if (any(names(cl) == "col")) {
	#stopifnot()
} 
#
sapply(res, function (x) {
	if (class(x) == "ellipsoid") {
		lines(predict(x), ...) # col = col)
	} else {
		points(x, ...) # col = col, cex = 2)
	}
	})

return(invisible(cl))
}

#if (!isGeneric("ellipsoidhull")) {
setGeneric("ellipsoidhull",
	function (x, ...)
		standardGeneric("ellipsoidhull")
)
#}

setMethod("ellipsoidhull",
	signature(x = "VegsoupPartition"),
	function (x, ...)
	.ellipsoidhull(x, ...)
)