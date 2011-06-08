#	understands formal arguments order.layer and abundant.2.top
#	perform reordering and 
LaTex.species <- function(x, type = c("free", "vegbase"), order.layer, abundant.2.top, abundance.treshold = 0.5, tabdev.table, spread.list)
{
#	x = object of class vegbase.species
#	type = "free" allows for some free formatting of
#	abbreviation and layer conventions, experimental
#	order.layer = arrange table by layers
#	abundant.2.top = rearange table.method to bring abundant
#		species to the table head.
#	tabdev.table = tabdev object returned from vegbase.Latex

debug = FALSE

if (debug)
{
	x = spc.part; type = "vegbase"; order.layer = FALSE
	abundant.2.top = TRUE
	tabdev.table = partition@tabdev
	spread.list = partition@spread
}
if (!inherits(x, "vegbase.species"))
	stop("Need object of class vegbase.species")

if (!inherits(tabdev.table, "tabdev"))
	stop("Need object of class tabdev")

if (!inherits(spread.list, "list"))
	stop("Need object of class tabdev")	
type <- match.arg(type)
abbr.layer <- names(x)

#	deparse compound taxon abbreviation and layer string
switch(type, free = {
	abbr <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.abbr.free)
	layer <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.layer.free)
#	cat("use type free: ")
#	cat("abbr:")
#	str(abbr)
#	cat("layer:")
#	str(layer)
	}, vegbase = {
	abbr <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.abbr)
	layer <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.layer)
#	cat("use type vegbase: ")
#	cat("abbr:")
#	str(abbr)
#	cat("layer:")
#	str(layer)
	})

taxa <- x@taxonomy$taxon[match(abbr, x@taxonomy$abbr)]

freq <- apply(getBin(x) > 0, 2, sum)

tdev <- tabdev.table$spcdev[match(names(x),
	tabdev.table$spcdev$species),]

sig <- tdev$deviance < 0.05
sig[sig] <- "*"
sig[sig == "FALSE"] <- ""
names.tdev <- tdev$species
tdev <- paste(round(tdev$deviance, 2), sig)
names(tdev) <- names.tdev

#	spread.list <- keep
spread.list <- spread.list[match(names(x),
	names(spread.list))]

spread <- unlist(sapply(spread.list,
	function (x) paste(sort(unique(x)),
	collapse = "-", sep = "")))

res <- new("LaTex.species",
	species = t(x@raw),
	freq = freq, # sort(freq, decreasing = TRUE),
	tabdev = tdev,
	spread = spread,
	taxa = taxa,
	abbr = abbr,
	layer = layer,
	raw = x@raw,
	vdm = x@vdm,
	pa = x@pa,
	taxonomy = x@taxonomy,
	query = x@query)

#	reordering

ord <- c("species", "taxa", "abbr", "freq", "tabdev",
		"spread", "layer", "raw", "vdm", "pa")
		
if (!abundant.2.top && order.layer) {
		cat("\n order layer")
	lay  <-  order(res@layer, decreasing = TRUE)
	for (i in ord) {
		slot(res, i) <- reorder.species.lay(res, i, lay)
	}
}

if (abundant.2.top && !order.layer) {
	cat("\n reorder abundant species to top")
	for (i in ord) {
		slot(res, i) <- reorder.species.freq(res, i, freq)
	}
}

if (abundant.2.top && order.layer) {
	cat("\nno handle for abundant.2.top and order.layer")
	cat("\ndo nothing")
}

layer.rule <- res@layer
u.layer <- unique(layer.rule)
n.item <- c()

for (i in u.layer) {
	n.item <- c(n.item,
	length(layer.rule[sapply(layer.rule,
			function(x) x == i) == TRUE]))
}
ruler <- c()
for (i in 1:length(u.layer)) {
	ruler <- c(ruler, sum(n.item[1:i]))
}

#	drop last ruler position is equal to bottom rule
ruler <- ruler[-length(ruler)]

treshold <- max(res@freq) * abundance.treshold
dots <- which(res@freq >= treshold)
attr(dots, "treshold") <- treshold

res@dots <- dots
res@ruler <- ruler

return(res)	
}