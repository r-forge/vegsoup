#	A typesetting engine for vegetation tables in phytosociology
#	x = object of class Vegsoup.partition
#	y = object of class Vegsoup.sites
#	part = index to select partition
#	path to folder were tex activities are defaults to 
#	verbose = print something on console
#	arrange.method = "dca" or "hclust"
#		passed from function Vegsoup.species.partition
#		to Vegsoup.species.arrange
#	split.table = not implemented
#	abundant.2.top = rearange method to bring abundant
#		species to the table head, passed to
#		function LaTex.species.
#	abundance.treshold = defines proportion of species
#		occurences in full data set
#	order.layer = arrange table by layers, passed to
#		function LaTex.species.
#	add.rule = Add table rule in tex file
#	drop.prefix = omit characters in plot names if applicable
#	rotate.labels = ratated labels in tex file to achieve narrow
#		collumns
#	txpwidth = column width in points for species in tex file
#	pwidth = column width of plot columns
#	longtable = use longtable environment in tex.
#	add.syntaxon = fill table caption with plant communities
#		present in table
#	apply.coverscale = experimental, transform cover values
#		to strings suitable for for typesetting
#	check.out = return matrix in as plain table suitable
#		for dump to spreadsheet formats
#	tag.species = logical, if true adds a numeric frequency summary
#		in tex file.
#	tag.treshold = set a treshold if tag.species evaluates to TRUE
#		otherwise equals abundance.treshold
#	check.out = logical default to TRUE and does not include
#		typesetting extras. Run with minimal formatting options.
#

# a number specifing frequency species occurence
# threshold in full data set
# range from 0 to inf, adds a frequency description
# in tex file, zero adds tag to all species!
# tag.treshold = 0,
# a proportion, prints dooted filled lines per table
# on full data set
# range from 0 to 1
# abundance.treshold = 0.5

#	understands formal arguments order.layer and abundant.2.top
#	perform reordering and

#	understands formal arguments order.layer and abundant.2.top
#	perform reordering and


#	helper functions
reorder.species.freq <- function (x, y, ind) {
tmp <- slot(x, y)
if (y == "species") {
	tmp <- tmp[order(-ind),]
	} else {
	tmp <- tmp[order(-ind)]		
	}
return(tmp)
}

reorder.species.lay <- function (x, y, ind) {
tmp <- slot(x, y)
if (y == "species") {
	tmp <- tmp[ind,]
	} else {
	tmp <- tmp[ind]		
	}
return(tmp)
}

#	retrieves number of occurences of species in dataset.
#	function retrieves statistics based on slot query,
#	maybe turn into method for class LaTex.species

commons <- function (x, treshold) {
#	x = tex.species
if (!inherits(x, "LaTex.species"))
	stop("Need object of class LaTex.species")
if (missing(treshold))
	cat("\ntry to find treshold")

spc <- slot(x, "query")$abbr

occ <- aggregate(rep(1, length(spc)),
		by = list(spc), sum)
names(occ) <- c("abbr", "occu")

if (missing(treshold))
{
	treshold  <-  fivenum(occ$occu)[4]
	cat("\n use", treshold, "as abundance treshold")
}

tmp <- occ[occ$occu >= treshold,1]
res <- match(tmp, occ$abbr)
res <- list(commons = res, treshold = treshold,
	occu = occ)
return(res)
}

.VegsoupDataLaTexSpecies <- function(x, type = c("free", "vegbase"), order.layer, abundant.2.top, abundance.treshold = 0.5, tabdev.table, spread.list, arrange.method = "dca")
{
#	x = object of class vegbase.species
#	type = "free" allows for some free formatting of
#	abbreviation and layer conventions, experimental
#	order.layer = arrange table by layers
#	abundant.2.top = rearange table.method to bring abundant
#		species to the table head.
#	tabdev.table = tabdev object returned from VegsoupDataPartition

debug = FALSE

if (debug) {
	x = prt
	part = 2; tex.part = getPartition(x, part)
	type = "vegbase"; order.layer = FALSE
	abundant.2.top = TRUE
	abundance.treshold = 0.5
	tabdev.table = x@tabdev
	spread.list = x@spread
} else {
	if (!inherits(x, "VegsoupDataPartition"))
		stop("Need object of class VegsoupDataPartition")

	if (!inherits(tabdev.table, "tabdev"))
		stop("Need object of class tabdev")

	if (!inherits(spread.list, "list"))
		stop("Need object of class tabdev")	
}
#	perform table rearrangement
x <- VegsoupDataArrange(x, arrange.method)

if (missing(type))
{
	type <- "vegbase"
	cat("\n use vegbase standard taxa coding")
} else {
	type <- match.arg(type)
}
abbr.layer <- names(x)

#	deparse compound taxon abbreviation and layer string
switch(type, free = {
	abbr <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.abbr.free)
	layer <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.layer.free)
#	cat("use type free: ");	cat("abbr:"); str(abbr); cat("layer:"); str(layer)
	}, vegbase = {
	abbr <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.abbr)
	layer <- sapply(strsplit(abbr.layer, ".", fixed = TRUE),
			   get.layer)
#	cat("use type vegbase: "); cat("abbr:"); str(abbr); cat("layer:"); str(layer)
	})

taxa <- x@taxonomy$taxon[match(abbr, x@taxonomy$abbr)]

freq <- apply(as.binary(x) > 0, 2, sum)

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

res <- new("VegsoupDataLaTexSpecies",
	species = t(x@species.raw),
	caption = "Empty",
	ruler = 1,
	dots = 1,
	freq = freq, # sort(freq, decreasing = TRUE),
	tabdev = tdev,
	spread = spread,
	taxa = taxa,
	abbr = abbr,
	layer = layer)
	

#	reordering
slots <- c("species", "taxa", "abbr", "freq", "tabdev",
		"spread", "layer")
		
#if (!abundant.2.top && order.layer) {
		cat("\n order layer, use layer standard ordering")
	layer.std <- cbind(layer = c("ml", "hl", "sl", "tl2", "tl1"),
	order = c(1,2,3,4,5))
	lay <- res@layer
	keep <- lay
	for (i in 1:nrow(layer.std))
		lay <- gsub(layer.std[i,1], layer.std[i,2], lay)

	lay <- order(lay, decreasing = TRUE)	
	
	for (i in slots) {
		slot(res, i) <- reorder.species.lay(res, i, lay)
	}
#}

if (abundant.2.top && !order.layer) {
	cat("\n reorder abundant species to top")
	for (i in slots) {
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

ruler <- ruler[-length(ruler)] # last ruler position is equal to bottom rule

treshold <- max(res@freq) * abundance.treshold
dots <- which(res@freq >= abundance.treshold)
attr(dots, "treshold") <- abundance.treshold

res@dots <- dots
res@ruler <- ruler

res

}

.VegsoupDataLatexSites <- function (x, basic, basic.tex.names, extended, extended.tex.names, split.table = FALSE, type = "free", caption = "Empty", arrange.method = "dca") {

	#	vegbase minimum standard set of site variables
	basic.standard <- c("cov", "t1cov", "t2cov", "scov", "hcov", "mcov",
		"htl1", "htl2", "hsl", "hhl1", "hhl2",
		"plsx", "plsy", "expo", "slope")
	basic.standard.translate <- c("Deck.", "BS1", "BS2", "SS", "KS", "MS",
		"H. BS1", "H. BS2", "H. SS", "H. KS1", "H KS2",
		"b", "l", "Exp.", "Ink.")

#	cbind(basic.standard, basic.standard.translate)

#	debugging
debug = FALSE

if (debug) {
	x <- prt; split.table = FALSE; part = 1
	caption = "Empty"; arrange.method = "dca"
#	basic = basic.standard; basic.tex.names = basic.standard.translate
	basic = basic; basic.tex.names = basic.tex.names
} else {
	if (!inherits(x, "VegsoupDataPartition"))
		stop("Need object of class VegsoupDataPartition, ", 
			"possibly reordered")
	if (missing(basic)) {
		cat("\nplease supply a vector matching column names",
			"\nuse standard set:")
		basic <- basic.standard
		str(basic.standard)
	}
	if (missing(basic.tex.names)) {
		cat("\nuse default German translation in LaTex output",
			"for sites table")
		basic.tex.names <- basic.standard.translate
		str(basic.tex.names)
	}	
	if (missing(extended)) {
		cat("\nno split rule defined for additional sites data")
		extended <- NULL
	}	
	if (missing(extended.tex.names)) {
		cat("\nuse names in data frame in LaTex output for",
			"\nadditional sites table if there is any")
		extended.tex.names <- NULL
	}
}
#	perform table reordering
x <- VegsoupDataArrange(x, arrange.method)

#	split table
if (!split.table) {
	save.match <- match(basic, names(Sites(x)))
	if (any(is.na(save.match))) {
		cat("variable(s) that do not match:",
			basic[is.na(save.match)])
	}
	sites.basic <- Sites(x)[,save.match[!is.na(save.match)]]
}

if (split.table) {
	#	split up table, not used
	stop("Extened sites table not implemented")
	sites.extended <- tmp[,names(tmp) %in% extended]
	rownames(sites.extended) <- sites.extended$plot

	if (prod(dim(sites.extended)) > 0) {
		sites.extended <- sites.extended[match(names(y$pa)[-1],
									 sites.extended$plot)]
		dimnames(sites.extended)[[2]] <- extended.tex.names
	}
	} else {
		sites.extended <- NULL
}
		
#	create plot size column
pls.cols <- grep("pls", names(sites.basic))

if (length(pls.cols) == 2) {
	sites.basic[,pls.cols[1]] <-
		apply(sites.basic[pls.cols], 1,
			function(x) x[1] * x[2])
	names(sites.basic)[pls.cols[1]] <- "size" # "size"
	sites.basic <- sites.basic[,-pls.cols[2]]
	basic[pls.cols] <- "size"
	basic <- unique(basic)
	basic.tex.names[pls.cols] <- "Fläche"
	basic.tex.names <- unique(basic.tex.names)
}
	
#	replace zeros coerce to matrix!
sites.basic <- gsub(" ", "", as.matrix(sites.basic))
zeros <- apply(sites.basic, 2, nchar) == 1
sites.basic[zeros]  <-
	gsub("0", "--", sites.basic[zeros])
		
#	create pasted layer column for herb layer
hhl.cols <- grep("hhl", dimnames(sites.basic)[[2]])

if (length(hhl.cols) == 2) {	
		
		
		sites.basic[,hhl.cols[1]] <-
			apply(sites.basic[,hhl.cols], 1,
				function(x)	paste(x[1], " (",  x[2], ")", sep =""))
		sites.basic <- sites.basic[,-hhl.cols[1]]
		colnames(sites.basic)[hhl.cols[1]] <- "hhl"
		basic[hhl.cols] <- "hhl"
		basic <- unique(basic)
		basic.tex.names[hhl.cols] <- "H. KS"
		basic.tex.names <- unique(basic.tex.names)
		
}

cat("\nsites table names translation\n",
	paste(basic, basic.tex.names, sep = ": "))

#	apply new dimnames
save.match <- match(basic, dimnames(sites.basic)[[2]])

dimnames(sites.basic)[[2]][save.match[!is.na(save.match)]] <-
		basic.tex.names[save.match[!is.na(save.match)]]

tex <- sites.basic
tex.extended  <-  sites.extended

if (is.null(tex.extended)) {		
	res <- new("VegsoupDataLaTexSites",
		sites.latex = tex,
		sites.latex.extended = matrix(""),
		caption = caption)
} else {
	cat("extended tables not implemented yet!")
}

return(res)
	
}
 
.VegsoupDataLaTexConfigure <- function(x, part = 1, type = c("free", "vegbase"), order.layer, abundant.2.top, abundance.treshold = 0.5, tabdev.table, spread.list, longtable = TRUE, file.name, basic, basic.tex.names)
{

debug = FALSE

if (debug) {
	part = 1; x = prt; part = 1; type = "vegbase";
	order.layer = TRUE;	abundance.treshold = 0.5
	tabdev.table = x@tabdev; spread.list = x@spread
}

res.species <- .VegsoupDataLaTexSpecies(getPartition(x, part), type,
	order.layer, order.layer, abundance.treshold,
	tabdev.table, spread.list)
res.sites <- .VegsoupDataLatexSites(getPartition(x, part),
	basic, basic.tex.names)

#	more here?

res <- new("VegsoupDataLaTex",
	species = res.species,
	sites = res.sites,
	file.name = file.name,
	longtable = longtable,
	part = part,
	LaTex = list())

return(res)	
}

.VegsoupDataLaTexDrop <- function (x, font.size = "footnotesize", add.rule, booktabs = TRUE, output.scale = FALSE, tag.species = TRUE, order.layer = order.layer) {

debug = FALSE

if (debug) {	
	x = config; output.scale = FALSE;
	font.size = "footnotesize"; booktabs = TRUE
} else {
	if (!inherits(x, "VegsoupDataLaTex"))
		stop("Need object of class VegsoupDataLaTex")
	if (missing(add.rule))
		add.rule <- FALSE
}
split.table <- length(x@sites@sites.latex.extended) > 1
if (split.table) {
	res <- c(x@LaTex$sites[[1]],
		x@LaTex$sites[[2]],
		x@LaTex$species)
} else {
	res <- x@sites@sites.latex
}

has.tag  <- FALSE

#	currently unused
#	shoud be generated by .VegsoupDataLaTexConfigure
#	has.tag <- !is.null(attr(x@taxa, "tag"))

#if (output.scale)
#{
#	spc.drop <- LaTex.output.scale(x)
#} else {
#	spc.drop <- x@species@species
#	spc.drop[spc.drop == 0] <- "."
#}

layer <- x@species@layer
layer[duplicated(layer)]  <- "--"
#if (order.layer && add.rule)

tex.taxa <- paste(x@species@taxa, x@species@layer, sep = "@")

species.drop <- x@species@species
species.drop[species.drop == 0] <- "."
dimnames(species.drop)[[1]] <- tex.taxa
species.drop <- cbind(layer = layer, species.drop)

#if (has.tag)
#{
#	if (tag.species) {
#	dimnames(tmp)[[1]] <-
#		paste(attr(x@taxa, "tag"), x@layer, sep = "@")
#}
#}

#	drop LaTex vegetation table
latex(species.drop, x@file.name,
	longtable = x@longtable,
	lines.page = dim(species.drop)[1],
	multicol = FALSE, size = font.size,
	booktabs = booktabs,
	rowlabel = "Nr.",
	caption = paste("Cluster ", x@part, ": ",
		x@species@caption, sep = ""),
	)
#	drop LaTex sites table basic slot tex
latex(x@sites@sites.latex,
	paste(x@file.name, "_sites_basic", sep = ""),
	longtable = TRUE,
	lines.page = dim(x@sites@sites.latex)[1],
	multicol = FALSE, size = font.size,
	booktabs = booktabs,
	rowlabel = "Nr.",
	caption = paste("Cluster ", x@part, ": ",
		x@sites@caption, sep = ""),
	)

#	drop Tex sites extended slot tex.extended
if (split.table) {
	latex(y@sites.extended,
		paste(file.name, "_sites_extended", sep = ""),
		longtable = FALSE,
		lines.page = dim(y@sites.extended)[1],
		multicol = FALSE, size = font.size,
		booktabs = booktabs,
		label = paste("vtb", i, sep = ""),
		rowlabel = "Nr.",
		caption = paste("Cluster ", x@part, ": ",
			cap, sep = ""))
}

return(x)

}

.VegsoupDataLaTexParse <- function (x) {
#	x = tex.drop
if (!inherits(x, "VegsoupDataLaTex"))
	stop("Need object of class VegsoupDataLaTex")

split.table <- length(x@sites@sites.latex.extended) > 1
tmp <- x

con <- file(paste(x@file.name, ".tex", sep = ""))
	species <- readLines(con)
close(con)

con <- file(paste(paste(x@file.name, "_sites_basic", sep = ""), ".tex", sep = ""))
	sites <- readLines(con)
close(con)

#	clean up, use UNIX specific system commands
system("rm ./Tex/*sites_basic.tex")

if (split.table) {
	con <- file(paste(paste(x@file.name, "_sites_extended",
		sep = ""), ".tex", sep = ""))
	sites.extended <- readLines(con)
	close(con)
	sites <- list(sites = sites,
		sites.extended = sites.extended)
	} else {
	sites  <- list(sites = sites, sites.extended = character(0))
}

x@LaTex <- list(species = species, sites = sites)
return(x)
	
}

#	post processing of latex ouput
#	generated by function latex() in library(Hmisc)
#	understands pwidth, txpwidth, rotate.labels
.VegsoupDataLaTexPost <- function (x, pwidth, txpwidth, rotate.labels, add.rule)
{
	
debug = FALSE

if (debug)
{	
	x = tex.parse
	txpwidth = 70; pwidth = 2	
	add.rule = TRUE; rotate.labels = TRUE
}
	
if (!inherits(x, "VegsoupDataLaTex"))
	stop("Need object of class LaTex.vegbase")	

tex.in  <- x@LaTex$species
longtable <- x@longtable	

tabular <- grep(ifelse(longtable,
	"begin{longtable}", "begin{tabular}"),
				tex.in, fixed = TRUE)
p.cols <- tex.in[tabular]
split.table <- length(x@sites@sites.latex.extended) != 0

if (longtable) {
	p.cols.cut <- (gregexpr("caption", p.cols)[[1]]) - 3
		} else {
	p.cols.cut <- max(gregexpr("}", p.cols)[[1]])
}

#	adjust column widths
#	adjust all but taxa column
p.cols.tab <- substring(p.cols, 1, p.cols.cut)
p.cols.cap <- substring(p.cols, p.cols.cut + 1, nchar(p.cols))
p.cols.tab.cut <- max(gregexpr("{", p.cols.tab, fixed = T)[[1]])
p.cols <- substring(p.cols.tab, p.cols.tab.cut, p.cols.cut)
p.cols.com <- substring(p.cols.tab, 1, p.cols.tab.cut - 1)
p.cols <- gsub("l", paste("p{", pwidth,"mm}", sep = ""),
					p.cols, fixed = TRUE)

#	adjust column width for taxa names
tx.col <- gregexpr(paste("p{", pwidth,"mm}", sep = ""),
					p.cols, fixed = TRUE)[[1]][1:3]
plot.cols <- substring(p.cols, tx.col[2], nchar(p.cols))
p.cols <- paste("{p{", txpwidth, "mm}p{6mm}", plot.cols, sep = "")
p.cols <- paste(p.cols.com, p.cols, p.cols.cap,
	collapse = "", sep = "")

tex.in[tabular] <- p.cols

cols <- dimnames(x@species@species)[[2]]

#	rotate columns labels
if (rotate.labels) {
	for (r in 1:length(cols)) {
		to.rotate <- gregexpr(cols[r], tex.in, fixed = TRUE)
		to.rotate <- sapply(to.rotate, function(x) x > 0)
		rotate <- tex.in[to.rotate]
		rotate <- gsub(cols[r],
			paste("\\\\begin{sideways}",
			cols[r],
			"\\\\end{sideways}",sep = ""),
			rotate)
		tex.in[to.rotate] <- rotate
	}
}

#	remove first line produced by latex() in library(Hmisc)
tex.in <- tex.in[-1]

#	remove layer suffix
ind <- grep("@", tex.in)
tmp <- tex.in[ind]
for (i in unique(x@species@layer)) {
	tmp <- gsub(paste("@", i, "&", sep = ""), "&", tmp)
}
tex.in[ind] <- tmp

#tmp <- strsplit(tmp, "@", fixed = TRUE)
#tmp <- sapply(tmp, function (x) {
#	res <- paste(x[1], substr(x[2], 3, nchar(x[2])+ 0), sep = "")
#	})


if (add.rule) { 
	rule.pos <- x@species@ruler
	if (length(rule.pos) > 0) {
		table.start <- grep(x@species@taxa[1], tex.in)[1]
		rule.pos <- table.start + rule.pos
		tex.in[rule.pos] <- paste("\\midrule", tex.in[rule.pos])
	}
}

#	change language
tex.in <- gsub("&layer&", "&Schicht&", tex.in, fixed = TRUE)
tex.in <- gsub("&tl1&", "&BS1&", tex.in, fixed = TRUE)
tex.in <- gsub("&tl2&", "&BS2&", tex.in, fixed = TRUE)
tex.in <- gsub("&sl&", "&SS&", tex.in, fixed = TRUE)
tex.in <- gsub("&hl&", "&KS&", tex.in, fixed = TRUE)
tex.in <- gsub("&ml&", "&MS&", tex.in, fixed = TRUE)

x@LaTex$species <- tex.in
#	groome \tabularnewline command
x@LaTex$species <- gsub("tabularnewline*", "tabularnewline",
	x@LaTex$species, fixed = TRUE)

x@LaTex$sites$sites <- gsub("tabularnewline*", "tabularnewline",
	x@LaTex$sites$sites, fixed = TRUE)
return(x)
}
	
VegsoupDataLaTexPipe <- function(x, part, file.name, verbose = TRUE, arrange.method = "dca", split.table = FALSE, abundant.2.top = FALSE, abundance.treshold = 0.5, tag.species = FALSE, order.layer = TRUE, add.rule = TRUE, drop.prefix = FALSE, rotate.labels = TRUE, output.scale = FALSE, txpwidth = 50, pwidth = 1, longtable = TRUE, add.syntaxon = FALSE, apply.coverscale = FALSE, check.out = TRUE, tag.treshold = 3, basic, basic.tex.names, type = "vegbase") {

#	debugging
debug = FALSE

if (debug) {
	x = prt; part = 1
	file.name <- "part"
	abundant.2.top = FALSE; add.rule = FALSE
	order.layer = FALSE; arrange.method = "dca"
	verbose = TRUE; tag.species = FALSE
	tag.treshold = 1
	txpwidth = 90; pwidth = 2; rotate.labels = TRUE
	abundance.treshold = 0
	type = "vegbase" # rises an error if not set!
	basic = basic
	basic.tex.names = basic.tex.names
	output.scale = FALSE
} else {
	if (missing(x))
		stop("\nplease support object of class VegsoupDataPartition")
	if (missing(part))
		stop("\nplease support a partition subset")
	if (missing(file.name) || is.null(file.name)) {
		cat("\nUse check out mode on")
		check.out <- TRUE
	} else {
		check.out <- FALSE
		file.name <- paste("./Tex/part", part, sep = "")
	}
	if (missing(abundance.treshold))
		abundance.treshold <- 0	# omit dotted lines 
	if (missing(tag.treshold))
		tag.treshold <- abundance.treshold
}	
config <- .VegsoupDataLaTexConfigure(x, part, type = type,
	order.layer = order.layer,
	abundance.treshold = abundance.treshold,
	abundant.2.top = abundant.2.top,
	tabdev.table = x@tabdev,
	spread.list = x@spread,
	file.name = file.name,
	basic = basic,
	basic.tex.names = basic.tex.names)

#	drop formatted tables
tex.drop <- .VegsoupDataLaTexDrop(config,
	add.rule = add.rule, tag.species = tag.species,
	output.scale = output.scale, order.layer = order.layer)

#	read file
tex.parse <- .VegsoupDataLaTexParse(tex.drop)

#	apply post fixes
tex.post <- .VegsoupDataLaTexPost(tex.parse,
	pwidth = pwidth, txpwidth = txpwidth,
	rotate.labels = rotate.labels, add.rule = add.rule)

#	drop final result
con <- file(paste(tex.post@file.name, ".tex", sep = ""), open = "w")
	writeLines(tex.post@LaTex$sites$sites, con)
	writeLines(tex.post@LaTex$species, con)

close(con)	

#res <- list(tex.species, tex.sites)
#res <- list(species = vegbase.LaTex.Checkout(res[[1]]),
#	sites = vegbase.LaTex.Checkout(res[[2]]))

return(invisible(tex.post))
}