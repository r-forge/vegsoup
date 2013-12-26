.plotVegsoupPartition <- function (x, y, ...) {
	#	x = prt
#	op <- par()
#	on.exit(par(op))
	
	if (!inherits(x, "VegsoupPartition")) stop
	cat("\nLet me calculate capscale first ...")
	cat("\nuse distance:", x@dist)

	#	capscale has difficulties when using community matrix in the formula
#	tmp <- 
	#cat(class(tmp))
	ord <- capscale(as.logical(x) ~ 1, data = Sites(x))
	#	number of axes shown in plot, default to frist 3
	axs <- matrix(c(1,2,1,3,2,3), 3,2, byrow = TRUE)
	
	#	ordisplom like display
	axs <- matrix(c(
		1,3,  2,3,  3,NA,
		1,2,  2,NA,	3,2,
		1,NA, 2,1,	3,1),
		9, 2, byrow = TRUE)	

	if (missing(y)) {
		#	nuse slot partition
		grp <- as.factor(Partitioning(x))
	}
	else {
		stop("no method yet")
	}
#	if (!missing(y) && missing(ind)) {
#		stop("please supply sites column name or index!")
#	}
#	if (!missing(y) && !missing(ind)) {
#		cat("\nnot implemented yet")
#	#	grp <- get.sites.variable(y, ind)	
#	}

	scs <- scores(ord, display = "sites")

	if (!identical(dim(scs)[1], length(grp))) {
		cat("\nremoved some sites!")
		grp <- grp[match(rownames(scs), names(grp)),1]
		if (getK(x) <= 11) {
			#	Suggests:
			require(RColorBrewer)
			pal <- brewer.pal(getK(x), "Spectral")
		} else {
			pal <- rep(rgb(0,0,0,.2), getK(x))
		}
		nlevels(grp)
	}

	par(mfrow = c(3,3), mar = rep(0,4))
				scs <- scores(ord,
					choices = unique(c(axs))[!is.na(unique(c(axs)))])
					
	apply(axs, 1, function (x) {
		if (!any(is.na(x))) {
			# x  <- axs[1,]
			lims <- range(scs$sites)
			fig <- ordiplot(ord, choices = c(x),
				type = "n", axes = FALSE,
				xlim = lims, ylim = lims)
			text(x = 0, y = par("usr")[3] - c(par("usr")[3] * 0.04),
				colnames(scs$sites)[x[1]])
			text(x = par("usr")[1] - c(par("usr")[1] * 0.04), y = 0,
				colnames(scs$sites)[x[2]], srt = 90)
			cents <- try(
				ordiellipse(fig, groups = grp, 
					choices = c(x), conf = .95,
					draw = "polygon", lty = 0,
					col = rgb(0,0,0,.1)), #
				silent = TRUE)

			points(fig, "sites")
	
			ordispider(fig, groups = grp, choices = c(x),
				lwd = c(1/.75)* .25, col = "white")
				
			if (class(cents) != "try-error") {
				labs <- sapply(cents, function (x) {
				c(x = x$center[1], y = x$center[2])
				})
				text(x = labs[1,], y = labs[2,],
					labels = dimnames(labs)[[2]],
					font = 2, cex = 2, col ="white")
			}	
		} else {
			frame()
			plot.window(c(-1,1), c(-1,1))
			text(0,0, labels = colnames(scs$sites)[x[1]], cex = 2)
		}
	})
	return(invisible(ord))
}

#	plot method
setMethod("plot",
	signature(x = "VegsoupPartition", y = "missing"),
	.plotVegsoupPartition
)


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