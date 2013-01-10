".coldiss" <- function (obj, colors, byrank = TRUE, diag = FALSE, ...) {
# adapted for vegsoup from function coldiss()
# Color plots of a dissimilarity matrix, without and with ordering
#
# License: GPL-2 
# Author: Francois Gillet, August 2009	
	require(gclus)

	if (missing(colors)) {
		cr <- cm.colors(4)
	} else {
		if (is.numeric(colors) & length(colors) == 1) {
			cr <- cm.colors(colors)		
		} else {
			cr <- colors
		}
	}

	print(class(obj))		
	D <- getDist(obj)

	D.cr = dmat.color(1 - D,
		byrank = ifelse(byrank, TRUE, FALSE),
		colors = cr)

	D.order = order.single(1 - D)
	D.cr.order = D.cr[D.order, D.order]

	op = par(mfrow = c(1,2), pty = "s")
	

	if (diag) {
		plotcolors(D.cr, rlabels = attributes(D)$Labels, 
			main = "Dissimilarity Matrix", 
			dlabels = attributes(D)$Labels)
		plotcolors(D.cr.order, rlabels = attributes(D)$Labels[D.order], 
			main = "Ordered Dissimilarity Matrix", 
			dlabels = attributes(D)$Labels[D.order])
	} else {
		plotcolors(D.cr, rlabels = attributes(D)$Labels, 
			main = "Dissimilarity Matrix")
		plotcolors(D.cr.order, rlabels = attributes(D)$Labels[D.order], 
			main = "Ordered Dissimilarity Matrix")
	}

	par(op)
}

#if (!isGeneric("heatmap")) {
setGeneric("coldiss",
	function (obj, ...)
		standardGeneric("coldiss")
)
#}
setMethod("coldiss",
    signature(obj = "VegsoupData"),
    function (obj, ...) .coldiss(obj, ...)
)