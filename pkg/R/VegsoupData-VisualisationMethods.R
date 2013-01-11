".coldiss" <- function (obj, colors, byrank = TRUE, diag = FALSE, ordered.only = FALSE, translate = FALSE, ...) {
# adapted for vegsoup from function coldiss()
# Color plots of a dissimilarity matrix, without and with ordering
#
# License: GPL-2 
# Author: Francois Gillet, August 2009
#
#	argument nc renamed to colors with extended meaning	
	require(gclus)

	cl <- match.call()
	
	if (missing(colors)) {
		cr <- cm.colors(4)
	} else {
		if (is.numeric(colors) & length(colors) == 1) {
			cr <- cm.colors(colors)		
		} else {
			cr <- colors
		}
	}
			
	D <- getDist(obj, ...)
	D.labels <- attributes(D)$Labels
	
	if (translate & any(names(cl) == "mode")) {
		D.labels <- DecomposeNames(obj)[D.labels, ]$taxon
		op <- par(xpd = NA)
	}

	D.cr <- dmat.color(1 - D,
		byrank = ifelse(byrank, TRUE, FALSE),
		colors = cr)

	D.order <- order.single(1 - D)
	D.cr.order <- D.cr[D.order, D.order]
	
	if (!ordered.only) op <- par(mfrow = c(1,2), pty = "s")	

	#	sub title
	sub <- paste("vegdist:", vegdist(obj), "\n",
		"decostand:",decostand(obj))
	
	if (diag) {
		if (!ordered.only) {
			plotcolors(D.cr,
				rlabels = D.labels, dlabels = D.labels,
				main = "Dissimilarity Matrix")
			title(sub = sub)
		}
		if (translate) {
			plotcolors(D.cr.order,
				#rlabels = NULL,
				dlabels = D.labels[D.order],
				main = "Ordered Dissimilarity Matrix")			
		} else {
			plotcolors(D.cr.order,
				rlabels = D.labels[D.order], dlabels = D.labels[D.order],
				main = "Ordered Dissimilarity Matrix")			
		}	
		title(sub = sub)				
	} else {
		if (!ordered.only) {
			plotcolors(D.cr, rlabels = D.labels, 
				main = "Dissimilarity Matrix")
			title(sub = sub)
		}				
		plotcolors(D.cr.order, rlabels = D.labels[D.order], 
			main = "Ordered Dissimilarity Matrix")
		title(sub = sub)			
	}

	if (!ordered.only) par(op)
	if (translate & any(names(cl) == "mode")) par(op)
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