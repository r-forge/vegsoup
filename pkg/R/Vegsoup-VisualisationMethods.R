plotPCO <- function (x, display = "sites", thresh = 0.2, ...) {
	ord <- capscale(as.matrix(x) ~ 1, data = Sites(x))	
	fig <- ordiplot(ord, display = display, type = "n") # , ...
	ol <- outlier(x, thresh = thresh)
	points(fig, select = !ol, col = 1)
	points(fig, select = ol, col = 2, pch = "+")
	#orditorp(fig, select = ol, col = 2)	
}
