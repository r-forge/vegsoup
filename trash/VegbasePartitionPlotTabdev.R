vegbasePartitionPlotTabdev <- function (x)
{
require(graphics)
opar <- par()
on.exit(par(opar))
#	x <- partitions

k <- sapply(x, function (x) slot(x, "k"))
n.sig.spc <- sapply(x,
	function (x) nrow(summary(slot(x, "tabdev"))))
tot.dev <- 	sapply(x,
	function (x) slot(x, "tabdev")$totdev)

par(mfrow = c(2,2))

plot(tot.dev ~ k, type = "b",
	xlab = "Number of cluster",
	ylab = "Total table deviance")
lines(spline(lowess(tot.dev ~ k, f = 2/3)), col = 2)

plot(n.sig.spc ~ k, type = "b",
	xlab = "Number of cluster",
	ylab = "Number of significant species")

lines(spline(lowess(n.sig.spc ~ k, f = 2/3)), col = 2)
	
plot(tot.dev ~ n.sig.spc, type = "n",
	xlab = "Number of significant species",
	ylab = "Total table deviance")
points(tot.dev ~ n.sig.spc, cex = 2.5)	
text(tot.dev ~ n.sig.spc, labels = k)	
lines(spline(lowess(tot.dev ~ n.sig.spc, f = 2/3)), col = 2)
}