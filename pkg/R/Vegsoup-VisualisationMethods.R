plotPCO <- function (x, display = "sites", ...) {
	ord <- capscale(as.matrix(x) ~ 1, data = Sites(x))	
	fig <- ordiplot(ord, display = display, type = "n") # , ...
	ol <- outlier(x, ...)
	points(fig, select = !ol, col = 1)
	points(fig, select = ol, col = 2, pch = "+")
	#orditorp(fig, select = ol, col = 2)	
}
