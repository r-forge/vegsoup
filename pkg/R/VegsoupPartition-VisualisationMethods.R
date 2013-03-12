#	to do!

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
x <- spread(x)	
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