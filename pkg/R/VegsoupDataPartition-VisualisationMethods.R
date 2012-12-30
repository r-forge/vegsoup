#	ellipsoidhull around partitions
.EllipsoidHull <- function (x, add = FALSE, col = "red", ...) {
#	x <- prt
res <- vector("list", length = getK(x))
for (i in 1:getK(x)) {
	xy <- as.matrix(coordinates(x)[Partitioning(x) == i,])
	if (nrow(xy) >= 3) {
		res[[i]] <- ellipsoidhull(xy)#, ...)
	} else {
		if (ncol(xy) > 1) {
			res[[i]] <- xy.coords(x = xy[, 1], y = xy[, 2])
		} else {
			res[[i]] <- xy.coords(x = t(xy)[, 1], y = t(xy)[, 2])		
		}
	}
}

if (!add) {
	plot(x@sp.points)
} else {
	points(x@sp.points)
}
sapply(res, function (x) {
	if (class(x) == "ellipsoid") {
		lines(predict(x), col = col)
	} else {
		points(x, col = col, cex = 2)
	}
	})

return(invisible(res))
}

#if (!isGeneric("EllipsoidHull")) {
setGeneric("EllipsoidHull",
	function (x, ...)
		standardGeneric("EllipsoidHull")
)
#}

setMethod("EllipsoidHull",
	signature(x = "VegsoupDataPartition"),
	.EllipsoidHull
)

#EllipsoidHull(prt)

#plot(pg)
#points(prt@sp.points, pch = "+")
#sapply(eh.prt, function (x) lines(predict(x), col = "red"))


.VegsoupPartitionConstancyHeatmap <- function (x, ...) {
#	x <- prt
if (!inherits(x, "VegsoupDataPartition"))
	stop("supply an object of class VegsoupDataPartition")
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
	distfun = function (x) vegdist(wisconsin(x), "bray"),
	cexRow = 0.5,
	scale = "none")
	
res <- res[ind$rowInd, ind$colInd]
res <- as.data.frame(t(res))
return(invisible(res))
}

.VegsoupPartitionSpreadHeatmap <- function (x, ...)
{
#	x <- prt
if (!inherits(x, "VegsoupDataPartition"))
	stop("supply an object of class VegsoupDataPartition")

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