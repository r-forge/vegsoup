#	plotting method for class species


#	implement table deviance!


vegbase.duleg <- function (x, k, dist = "bray", deco = FALSE, grain = 1, method = "pam", indicator = "duleg")
{

debug = FALSE	
if (debug == TRUE)
{
	x = spc; dist = "bray"; deco = FALSE; k = 5
	method = "pam"; indicator = "isamic"
}

if (!inherits(x, "vegbase.species"))
	stop("Need object of class vegbase.species")

if (missing(k))
	stop("Supply a value of k")
	
if (missing(dist))
	dist = "bray"

if (missing(deco))
{
	dis <- vegdist(x@pa, dist)
} else {
	dis <- vegdist(wisconsin(x@pa), dist)
}

sig.duleg <- function (x, p = 0.05, ...)
{# x = tmp
	if (!inherits(x, "duleg"))
		stop ("need object of class duleg")
	tmp <- data.frame(x$maxcls[x$pval <= p],
		   	round(x$indcls[x$pval <= p], 4),
   			x$pval[x$pval <= p])
	names(tmp) <- c("cluster", "ind", "prob")
	tmp <- tmp[order(tmp$cluster, -tmp$ind), ]
	tmp
}

clus.ind <- function (dis, x, k, grain, method = "pam", indicator = "duleg")
{
	switch(method, pam = {
		clust <- pam(dis, k, diss = TRUE)
		clust <- clust$clustering
		},
		agnes = {
		clust <- cutree(agnes(dis, diss = TRUE), k)
		names(clust) <- attr(dis, "Labels")
		},
		hclust = {
		clust <- cutree(hclust(dis, "ward"), k)
		}
	)
	mem <- clust
	tmp <- sapply(1:k, function (x)
		length(mem[clust == x]))
	out <- mem %in% c(1:k)[tmp < grain + 1] 

	if (any(out))
	{
		out <- mem[out]
	} else {
		out <- vector("numeric", 0)
	}
	switch(indicator, duleg = {	
	cat("\n... use indicator duleg")
	tmp <- duleg(x, clust, numitr = 100)
	sig <- sig.duleg(tmp)
	sig <- c(nrow(sig), fivenum(sig$ind))
	res <- round(apply(tmp$indval, 1, sum), 2)
	res <- list(res = res, sig = sig, out = out)
	},
	isamic = {
	cat("\n... use indicator isamic")
	res <- isamic(x, clust)
	sig <- rep(NA,6)
	res <- list(res = res, sig = sig, out = out)
	})
	
	res
}

res <- c()
sig  <- c()
out <- c()

cat("\n... compute partitions\n")

for(i in 1:k)
{
		cat(".")
		tmp <- clus.ind(dis, x@pa, i, grain,
			method, "duleg")
		out[[i]] <- tmp$out
		res.d <- cbind(res, tmp$res)
		sig.d <- cbind(sig, tmp$sig)
		tmp <- clus.ind(dis, x@pa, i, grain,
			method, "isamic")
		res.i <- cbind(res, tmp$res)
}



ncls <- sapply(out, function (x) length(x))
tmp <- ncls
cls <- (1:k)[tmp >= grain]
tmp[tmp >= grain]  <- cls
out <- cbind(k = tmp, out = ncls)

sig[is.na(sig)] <- 0
tmp <- res
res <- t(apply(res, 1, function (x) c(0, diff(x)) ))
	
res <- apply(res, 2,
	function(x)	c(sum(x[x > 0]), sum(x[x < 0])))

res <- rbind(res, apply(tmp, 2, fivenum),sig)

res <- new("vegbase.species.duleg",
	diff = res[1:2,],
	fivenum = res[3:7,],
	k = as.integer(k),
	sig = res[8:13,],
	outgroups = out)
return(res)
}