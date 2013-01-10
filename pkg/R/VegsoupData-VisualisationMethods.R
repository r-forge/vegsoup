# coldiss()
# Color plots of a dissimilarity matrix, without and with ordering
#
# License: GPL-2 
# Author: Francois Gillet, August 2009
#

# Usage:
#	coldiss(D = dissimilarity.matrix, nc = 4, byrank = TRUE, diag = FALSE)
#	If D is not a dissimilarity matrix (max > 1), then D is divided by max(D)

# Example:
# coldiss(spe.dj, nc=9, byrank=F, diag=T)

# byrank= TRUE		equal-sized categories
# byrank= FALSE		equal-length intervals
#	colors to interpolate; must be a valid argument to col2rgb().

heatmap <- function (obj, colors, byrank = TRUE, diag = FALSE) {
	require(gclus)

	#	colors = brewer.pal(3, "Spectral")
	if (missing(colors)) {
		cr <- cm.colors(4)
	} else {
		if (is.numeric(colors) & length(colors) == 1) {
			cr <- cm.colors(colors)		
		} else {
			cr <- colors
		}
	}
		
	D <- getDist(obj)

	if (max(D) > 1) D <- D / max(D)

	spe.color = dmat.color(1 - D,
		byrank = ifelse(byrank, TRUE, FALSE),
		colors = cr)

	spe.o = order.single(1 - D)
	speo.color = spe.color[spe.o, spe.o]

	op = par(mfrow = c(1,2), pty = "s")
	
	if (diag) {
		plotcolors(spe.color, rlabels=attributes(D)$Labels, 
			main="Dissimilarity Matrix", 
			dlabels = attributes(D)$Labels)
		plotcolors(speo.color, rlabels = attributes(D)$Labels[spe.o], 
			main="Ordered Dissimilarity Matrix", 
			dlabels = attributes(D)$Labels[spe.o])
	} else {
		plotcolors(spe.color, rlabels=attributes(D)$Labels, 
			main="Dissimilarity Matrix")
		plotcolors(speo.color, rlabels=attributes(D)$Labels[spe.o], 
			main="Ordered Dissimilarity Matrix")
	}

	par(op)
}
	require(RColorBrewer)
cols <- colorRampPalette(brewer.pal(11, "Spectral"))

heatmap(dta, cols(2))