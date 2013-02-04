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

#	alternative plotting methof
#	not a generic for plot

if (!isGeneric("rectangles")) {
setGeneric("rectangles",
	function (obj, plot, ...)
	standardGeneric("rectangles"))
}

setMethod("rectangles",
	signature(obj = "VegsoupPartition"),
	function (obj, plot, ...) {
	#	obj <- prt
	
	if (missing(plot)) {
		plot = TRUE	
	}
	p <- unique(Partitioning(obj))
	d <- dim(obj)
	
	#	subsetting will issue a warning
	#	this is harmless and not of interessent at this point
	op <- options()
	options("warn" = -1)
	res <- t(sapply(sort(p), function (x) {
		dim(obj[Partitioning(obj) == x, ])
		}))
	options(op)	
	
	res <- cbind(res, p)
	if (plot) {
	
	#	order
	r <- res[order(res[, 1], res[,2]), ]		
	plot(max(r[, 1]), max(r[, 2]),
		xlim = c(0, max(r[, 1])), ylim = c(0, max(r[, 2])),
		type = "n", bty = "n",
		xlab = "number of sites", ylab = "number of species",
		sub = paste("total number of sites and species:",
			d[1], d[2]
		))
	rect(0, 0, r[,1], r[, 2], ...)
	points(r[,1] , r[,2], pch = 16, col =" white", cex = 3)
	text(r[,1] , r[,2], labels = r[, 3], cex = 1, font = 2)
	rug(1, side = 1, lwd = 1)
	}
	
	return(res)
	}
)


.VegsoupPartitionConstancyHeatmap <- function (x, ...) {
#	x <- prt
if (!inherits(x, "VegsoupPartition"))
	stop("supply an object of class VegsoupPartition")
#	x = prt
k1 <- as.logical(x)
x <- x@spread	
k <- sort(unique(sapply(x, max)))

if (max(k) > 1) {
x <- sapply(x, table)

tmp <- vector("list", length = length(x))

for (i in seq(along = x)) {
	#	i = 1
	xi <- as.matrix(x[[i]])
	m <- matrix(0, max(k), 1)
	m[as.numeric(rownames(xi)),] <- xi
	colnames(m) <- names(x)[i]
	tmp[[i]] <- m
}

res <- matrix(0, max(k), length(tmp),
	dimnames = list(c(1:max(k)), sapply(tmp, colnames)))

for(i in 1:ncol(res)) {
res[,i] <- tmp[[i]]	
}

cols <- cm.colors(max(res))
cols[1] <- NA

#rownames(res) <- gsub(".kl", "", rownames(res), fixed = TRUE)
} else {
res <- k1
cols <- cm.colors(max(res))
cols[1] <- NA
}
res <- t(res > 0)
mode(res) <- "numeric"

cols <- c("grey80", "grey20")
ind <- heatmap(res,# col = cols,
 	hclustfun = function (x) hclust(x, method = "ward"),
	distfun = function (x) vegan::vegdist(wisconsin(x), "bray"),
	cexRow = 0.5,
	scale = "none")
	
res <- res[ind$rowInd, ind$colInd]
res <- as.data.frame(t(res))
return(invisible(res))
}

.VegsoupPartitionSpreadHeatmap <- function (x, ...)
{
#	x <- prt
if (!inherits(x, "VegsoupPartition"))
	stop("supply an object of class VegsoupPartition")

k1 <- as.logical(x)
x <- Spread(x)	
k <- sort(unique(sapply(x, max)))

if (max(k) > 1) {
x <- sapply(x, table)

tmp <- vector("list", length = length(x))

for (i in seq(along = x)) {
	#	i = 1
	xi <- as.matrix(x[[i]])
	m <- matrix(0, max(k), 1)
	m[as.numeric(rownames(xi)),] <- xi
	colnames(m) <- names(x)[i]
	tmp[[i]] <- m
}

res <- matrix(0, max(k), length(tmp),
	dimnames = list(c(1:max(k)), sapply(tmp, colnames)))

for(i in 1:ncol(res)) {
res[,i] <- tmp[[i]]	
}

cols <- cm.colors(max(res))
cols[1] <- NA

#rownames(res) <- gsub(".kl", "", rownames(res), fixed = TRUE)
} else {
res <- k1
cols <- cm.colors(max(res))
cols[1] <- NA
}
res <- t(res > 0)
mode(res) <- "numeric"

cols <- c("grey90", "grey10")
ind <- heatmap(res, col = cols,
 	hclustfun = function (x) hclust(x, method = "ward"),
	distfun = function (x) vegdist(wisconsin(x), "bray"),
	cexRow = 0.5,
	scale = "none")
	
res <- res[ind$rowInd, ind$colInd]
res <- as.data.frame(t(res))
return(invisible(res))
}