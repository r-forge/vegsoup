QueryTaxonomy <- function (x, y, file.x, file.y, csv2 = TRUE, pmatch = FALSE, return.species = TRUE, verbose = TRUE) {

#	input formats
test <- combn(c("x", "y", "file.x", "file.y"), 2)
cmb <- test <- test[, c(1, 3, 4, 6)]
tmp <- c(
	x = !missing(x), y = !missing(y),
	file.x = !missing(file.x), file.y = !missing(file.y))

for (i in seq(along = tmp)) {
	test[test == names(tmp[i])] <- tmp[i]
}

mode(test) <- "logical"

sel <- apply(test, 2, all)
if (all(sel == FALSE)) stop("please supply x respectively file.x and y respectively file.y")
if (sum(as.numeric(sel)) > 1) {
	cat("supplied", paste(cmb[, sel], collapse = " and "), "\n")
	stop("\ni don't know what to choose?")
}

if (which(sel) == 1) {
	species <- x
	taxonomy <- y		
}

if (which(sel) == 2) {
	species <- x
	if (csv2) {
		taxonomy <- read.csv2(file.y, stringsAsFactors = FALSE, check.names = FALSE)
	} else {
		taxonomy <- read.csv(file.y, stringsAsFactors = FALSE, check.names = FALSE)
	}
}

if (which(sel) == 3) {
	if (csv2) {
		species <- read.csv2(file.x, stringsAsFactors = FALSE, check.names = FALSE)
	} else {	
		species <- read.csv(file.x, stringsAsFactors = FALSE, check.names = FALSE)
	}
	taxonomy <- y
}

if (which(sel) == 4) {
	if (csv2) {
		species <- 	read.csv2(file.x, stringsAsFactors = FALSE, check.names = FALSE)
		taxonomy <- read.csv2(file.y, stringsAsFactors = FALSE, check.names = FALSE)
	} else {
		species <- 	read.csv(file.x, stringsAsFactors = FALSE, check.names = FALSE)
		taxonomy <- read.csv(file.y, stringsAsFactors = FALSE, check.names = FALSE)
	}
}

#	for safety if x is supplied as data.frame
species <- as.data.frame(as.matrix(species), stringsAsFactors = FALSE)
#	check names and bring to order
species.mandatory.columns <- c("plot", "abbr", "layer", "cov")

if (!all(species.mandatory.columns %in% names(species))) {
	stop("\n need mandatory columns ",
		paste(species.mandatory.columns, collapse = ", "),
		" in species data")	
} else {
	species <- species[species.mandatory.columns]	
}	

#	keep only two columns
taxonomy <- taxonomy[c("abbr", "taxon")]

#	check unique abbrevations
rownames(taxonomy) <- taxonomy$abbr
	
test1 <- match(unique(species$abbr), taxonomy$abbr)

if (any(is.na(test1))) {
	test1 <- unique(species$abbr)[is.na(test1)]
	cat(paste("the following abbrevation(s) used in",
	cmb[1,sel],
	"were not found in supplied reference list",
	cmb[2,sel],
	"\n"))
	print(test1)
	cat("did you mean?\n")
	test1.pmatch <- matrix(c(test1, taxonomy$abbr[pmatch(test1, taxonomy$abbr)]), ncol = 2)
	print(test1.pmatch)
	if (pmatch) {
		for (i in 1:nrow(test1.pmatch)) {
			species$abbr[species$abbr == test1.pmatch[i,1]] <- test1.pmatch[i,2]
		}
		cat("replaced", test1.pmatch[,1])	
	} else {
		cat("if that is correct you can force me to replace those abbreviations!")
		cat("\ncall the function again with option pmatch = TRUE")
	}
}

 
#	to do! validity method for vegsoup

test2 <- dim(species)[1] - dim(unique(species[,c(1:4)]))[1]

if (test2 > 0) {
	warning("\nspecies data not unique for ", test2, " sample(s)")
	if (verbose) {
		cat("\n")
		print(species[duplicated(species[ ,c(1:4)]), ])
	}
	species <- species[!duplicated(species[, c(1:4)]), ]
	warning("\nremoved duplicted species:\n\n")
} else {
	if (verbose) {
		cat("\nno duplicates found")
	}
}

res <- taxonomy[as.character(unique(species$abbr)), ]
if (any(is.na(res[, 1]))) {
	test3 <- as.character(unique(species$abbr))[is.na(res[, 1])]
	cat("\nnot found the following abbrevation(s) in supplied reference list\n")
	print(test3)
	#	to do!
	#	implement pmatch as above
	return(test1.pmatch)
	stop("Please review your reference list, you might need to include some new taxa")
}

if (any(is.na(species))) {
	warning("\n... NAs introduced")
	cat(apply(species, 2, function (x) any(is.na(x))) )
}

if (return.species == FALSE) {
	species <- NULL
}	

return(list(taxonomy = res, species = species))
}

#	reshape tables where layers are in seperate columns

ReshapeMultiCoverColumns <- function (x, file, layers, csv2 = TRUE, verbose = TRUE) {

if (missing(x) & missing(file)) {
	stop("please supply either a data frmae or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		if (csv2) {
			x <- read.csv2(file,
				stringsAsFactors = FALSE, check.names = FALSE)
		} else {
			x <- read.csv(file,
				stringsAsFactors = FALSE, check.names = FALSE)
		}
	}
} else {
	if (is.data.frame(x) & missing(file)) {
			#	for safety
			x <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)
		} else {
			stop("please supply a data.frame or use file argument")	
	}
}

if (missing(layers)) {
	layers <- as.character(layers)
	plot.abbr <- match(c("plot", "abbr"), names(x))
	layers <- names(x)[-plot.abbr]
	if (verbose) {
		cat("attempt to use columns:", layers, "as layer")	
	}
}

res <- x

layers <- data.frame(layers, index = match(layers, names(x)),
	stringsAsFactors = FALSE)

res <- c()
for (i in 1:nrow(layers)) {
	res <- rbind(res,
	cbind(layers[i, 1], as.matrix(x[, c(plot.abbr, as.integer(layers[i, 2]))])))
}

res <- as.data.frame(res,
	stringsAsFactors = FALSE)
res <- res[,c(2,3,1,4)]
names(res) <- c("plot", "abbr", "layer", "cov")

res <- res[res$cov != "0",]
res <- res[res$cov != "",]

res <- res[order(res$plot, res$abbr, res$layer),]

}

#	rename!
#	Shp2SitesLong to ReadOGR2SitesLong	
Shp2SitesLong <- function (dsn, layer, plot.column, elevation.column, round = TRUE, verbose = TRUE) {

if (missing(plot.column)) {
	stop ("please supply a column name in OGR data source indicating plot ids")
}

require(rgdal)

#if (missing(elevation.column)) {
#	elevation.column <- ""
#	if (verbose) {
#		cat("no column for elevations supplied")		
#	}
#}

#dsn = "/Users/roli/Dropbox/traunsee/dta/shp/pt_ts_plots"
#layer = "pt_ts_plots_epsg4326"
#plot.column = "NAME"
#elevation.column = "GPS_HEIGHT"
#	first check column names aigeinst ogrInfo
pt <- ogrInfo(dsn, layer)

pt.names <- pt$iteminfo$name
test <- match(c(plot.column, ifelse(missing(elevation.column), "", elevation.column)), pt.names)
if (any(is.na(test)) & verbose) {
	cat("\nogrinfo returns\n")
	print(pt)
	cat("you supplied: ", c(plot.column, ifelse(missing(elevation.column), "", elevation.column)))
	cat("\nI found only:",
		ifelse(length(pt.names[test[!is.na(test)]]) == 0,
			"... nothing?",
			pt.names[test[!is.na(test)]]))
}

pt <- readOGR(dsn, layer)

#	to do!
#	make CRS an argument to the function

pt <- spTransform(pt, CRS("+init=epsg:4326"))

if (missing(elevation.column)) {
	if (verbose) {
		cat("\nno column for elevations supplied")		
	}
	df <- data.frame(coordinates(pt),
		plot = as.character(pt@data[,names(pt) == plot.column]),
		stringsAsFactors = FALSE)
} else {
	df <- data.frame(coordinates(pt),
		altitude = as.numeric(as.character(pt@data[,names(pt) == elevation.column])),
		plot = as.character(pt@data[,names(pt) == plot.column]),
		stringsAsFactors = FALSE)
}

names(df)[1:2] <- c("longitude", "latitude")	

if (dim(coordinates(pt))[2] == 2 & missing(elevation.column)) {	
	res <- data.frame(as.character(df$plot),
		stack(df, select = 1:2),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	names(res) <- c("plot", "variable", "value")
	if (round) {
		res$value[res$variable == "longitude"] <-
			round(res$value[res$variable == "longitude"], 6)
		res$value[res$variable == "latitude"] <-
			round(res$value[res$variable == "latitude"], 6)
	}	
}

if (dim(coordinates(pt))[2] == 2 & !missing(elevation.column)) {
	res <- data.frame(as.character(df$plot),
		stack(df, select = 1:3),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	#	for safety
	res[,3] <- as.numeric(as.character(res[,3]))
	names(res) <- c("plot", "variable", "value")
	if (round) {
		res$value[res$variable == "longitude"] <-
			round(res$value[res$variable == "longitude"], 6)
		res$value[res$variable == "latitude"] <-
			round(res$value[res$variable == "latitude"], 6)
		res$value[res$variable == "altitude"] <-
			round(res$value[res$variable == "altitude"], 0)
	}	
}

if (dim(coordinates(pt))[2] == 3) {
	#	buggy!
	if (verbose) {
		cat("attempt to use z-dimension from OGR data source")
	}
	res <- data.frame(as.character(df$plot), stack(df, select = 1:3),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	names(res) <- c("plot", "variable", "value")
	if (round) {
		res$value[res$variable == "longitude"] <-
			round(res$value[res$variable == "longitude"], 6)
		res$value[res$variable == "latitude"] <-
			round(res$value[res$variable == "latitude"], 6)
		res$value[res$variable == "altitude"] <-
			round(res$value[res$variable == "altitude"], 0)
	}
}

return(invisible(res))

}

#	stack sites data frame to match database structure

SitesWide2SitesLong <- function (x, file, csv2 = TRUE, verbose = FALSE) {
#	file = "~/Documents/vegsoup-data/windsfeld dta/sites wide.csv"

if (missing(x) & missing(file)) {
	stop("please supply either a data frame or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		if (csv2) {
			x <- read.csv2(file,
				stringsAsFactors = FALSE, check.names = FALSE)
#			x <- read.csv2(file,
#				colClasses = "character", check.names = FALSE)

		} else {
			x <- read.csv(file,
				stringsAsFactors = FALSE, check.names = FALSE)
#			x <- read.csv2(file,
#				colClasses = "character", check.names = FALSE)
		}
	}
} else {
	if (is.data.frame(x) & missing(file)) {
		x <- x
		} else {
			stop("please supply a data.frame or use file argument")	
	}
}


#	all columns must be of mode character to  use stack()
res <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE, colClasses = "character")
res.stack <- stack(res, stringsAsFactors = FALSE)

plot <- res.stack[res.stack$ind == "plot",]$values
plot <- rep(plot, (nrow(res.stack)/length(plot))- 1)
res.stack <- res.stack[!res.stack$ind == "plot",]
res.stack <- data.frame(
	plot = as.character(plot),
	variable = as.character(res.stack[,2]),
	value = as.character(res.stack[,1]),
	stringsAsFactors = FALSE)
res.stack <- res.stack[order(res.stack$plot),]
res.stack[is.na(res.stack)] <- ""

if (any(res.stack$plot == "")) {
	stop("please review your data")
}
	
rownames(res.stack) <- 1:nrow(res.stack)
res <- res.stack

if (verbose) {
	print(unique(res$variable))
}	

return(invisible(res))
}

SRTM <- function (x) {
	if (!inherits(obj, "VegsoupData")) stop("Need object inheriting from class VegsoupData")
	require(geonames)
	res <- unlist(apply(coordinates(obj), 1, function (x) GNsrtm3(lat = x[2], lng = x[1])[1]))
}

MakeAbbr <- function (x)  {
	 x = tb$taxon
    names <- make.names(x, unique = FALSE)
    names <- lapply(strsplit(names, "\\."),
    function(x) if (length(x) > 1)
        substring(x, 1, 4)
    else x)
    names <- unlist(lapply(names,
    	function (x) if (length(x) > 1) 
        paste(x[seq(1, length(x))], collapse = " ")
    else x))
    names <- gsub("ssp  ", "", names, fixed = TRUE)
    names <- gsub("var  ", "", names, fixed = TRUE)
    names <- gsub("  ", " ", names, fixed = TRUE)
    names <- abbreviate(names, 8)
   	names <- make.names(names, unique = TRUE)
    names
}

#	convert between matrix formats for import
SpeciesWide2SpeciesLong <- function (x, file, csv2 = TRUE, verbose = FALSE) {

if (missing(x) & missing(file)) {
	stop("please supply either a data frame or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		if (csv2) {
			x <- read.csv2(file,
				colClasses = "character", check.names = FALSE)				
		} else {
			x <- read.csv2(file,
				colClasses = "character", check.names = FALSE)				
		}
	}
} else {
	if (is.data.frame(x) & missing(file)) {

		} else {
			stop("please supply a data.frame")	
	}
}

#	check schema
abbr <- grep("abbr", names(x))
layer <- grep("layer", names(x))
comment <- grep("comment", names(x))

if (length(abbr) > 0 & length(layer) > 0 & length(comment) > 0) {

res <- c()

for (i in c(max(c(abbr, layer, comment)) + 1):ncol(x)) {
	tmp <- data.frame(plot = names(x)[i],
		abbr = as.character(x$abbr),
		layer = as.character(x$layer),
		cov = as.character(x[,i]),
		comment = as.character(x$comment),
		stringsAsFactors = FALSE)
	
	res <- rbind(res, tmp)
}
res <- res[res$cov != "0",]
res <- res[res$cov != "",]
res <- res[res$cov != ".",]

if (length(grep(",", res$cov)) > 0) {
	res$cov <- gsub(",", ".", res$cov)
	if (verbose) {
		"\n... groomed decimals, replaced colons with dots"
	}

}
if (verbose) {
	test <- try(as.numeric(res$cov))
	if (class(test) != "try-error") {
		cat("\n... cover seems to be numeric")
		cat("\n    Tukey's five number summary:", fivenum(test))
	} else {
		cat("\n... cover seems to be categorical")
		print(table(res$cov))
	}
	
	cat("\n... data seems to have", length(unique(res$layer)), "layer:", unique(res$layer))
#	print(table(res$layer))
}
return(invisible(res))
} else {
	if (length(abbr) < 1) {
		warning("did not find column abbr")		
	}
	if (length(layer) < 1) {
		warning("did not find column abbr")		
	}
	if (length(comment) < 1) {
		warning("did not find column comment")		
	}
	stop("can't coerce object")
}
}

#	function to import monospaced commuity tables
verbatim <- function (file, colnames, layers, replace = c("|", "-", "–", "_"), verbose = TRUE, species.only = FALSE) {

require(stringr)

if (missing(file)) {
	stop("plaese supply a path to a file")
}

if (!missing(layers)) {
	if (!is.list(layers)) {
		stop("supplied argument layers must be a list")
	} else {
		stopifnot(length(names(layres)) == length(layres))
		has.layers <- TRUE
		l <- rep(names(layres), lapply(layres, function (x) diff(x) + 1))	
	}
} else {
	has.layers <- FALSE	
}
#	file <- "~/Dropbox/starzengruber1999/StarzengruberTab1.txt"

#	read file
txt <- readLines(file.path(file))

#	get and test keywords
hb <- grep("BEGIN HEAD", txt)
he <- grep("END HEAD", txt)
tb <- grep("BEGIN TABLE", txt)
te <- grep("END TABLE", txt)
hks <- c(hb, he, tb, te)

if (length(hks) != 4) {
	stop("did not find all keywords!")
}

#	test tabs
if (length(grep("\t", txt) > 0)) {
	stop("detected tab characters, please review your data.")	
}

#	replace
if (length(replace) > 0) {
	for (i in 1:length(replace)) {
		txt <- sapply(txt,
			function (x) gsub(replace[i], " ", x, fixed = TRUE),
			USE.NAMES = FALSE)
	}
	
}

#	find empty lines
el <- sapply(txt, nchar, USE.NAMES = FALSE)

#	find also uncomplete lines not representing data
#	for example, lines consiting of only spaces
ul <- el < median(el) & el != 0
#	hook keywords
ul[hks] <- FALSE
el <- el == 0
el[ul] <- TRUE

#	over long lines
txt <- str_trim(txt, side = "right")

#	select elements with species abundances
sel1 <- rep(FALSE, length(txt))
sel1[(tb + 1) : (te - 1)] <- TRUE
sel1[el] <- FALSE # omit empty lines

#	select elements with header entries
sel2 <- rep(FALSE, length(txt))
sel2[(hb + 1) : (he - 1)] <- TRUE
sel2[el] <- FALSE # omit empty lines

#	the table as a vector
#	of strings for each line
t.txt <- txt[sel1]
a.txt <- txt[sel2]

#	test
if (length(unique(sapply(t.txt, nchar))) > 1) {
	stop("length of characters",
		" is not the same for all lines!")
}

#	a mono spaced type matrix
#	each cell is a single glyph, space or dot
t.m <- matrix(" ", ncol = length(t.txt),
	nrow = max(sapply(t.txt, nchar)))
vals <- sapply(t.txt, function (x) {
			sapply(1:nchar(x),
				function (y) substring(x, y, y), USE.NAMES = FALSE)
		},
		simplify = FALSE, USE.NAMES = FALSE)
t.m[] <- unlist(vals)
t.m <- t(t.m)

#	search for the beginning of the data block
#	crude!
n.dots <- apply(t.m, 2,
	function (x) sum(sapply(x, function (y) y == ".")) )
first.dot <- which(n.dots != 0)[1]

txa <- str_trim(apply(t.m[, 1:(first.dot -1)], 1,
	function (x) paste(x, collapse = "")), side = "right")
val <- t.m[, first.dot: ncol(t.m)]

#	check for spaces as seperators
n.space <- apply(val, 2,
	function (x) sum(sapply(x, function (y) y == " ")) )
	
#	additional check for data integrity
test <- which(n.space != nrow(t.m) & n.space != 0)

if (verbose) {
	cat("found", nrow(t.m), "species")
}
	
if (length(test) > 0) {
	warning("some mono type character columns deviate from expected pattern\n")
	for (i in test) {
		#	missing dot
		if (length(grep(".", val[,i], fixed = TRUE)) > 0) {
			cat("\nmissing dot in species",
				txa[which(val[,i] == " ")],
				"in column", i + (first.dot - 1))	
		}
		#	misplaced value
		else {
			cat("\nmisplaced value",
				"in species",
				txa[which(val[,i] != " ")],
				"in column", i + (first.dot - 1))			
		}
		
	}
	stop("\nplease review your data and apply changes")
} else {
	if (verbose) {
		cat("\nfound no obvious errors in species data block",
			"\nskip", table(n.space)[2], "columns of blanks",
			"and retain", dim(val[, n.space == 0])[2], "columns of data")	
	}
}

#	and omit 
if (any(n.space == 0)) {
	val <- val[, n.space == 0]
}

if (verbose) {
	cat("\ncharacters found in the species data block:",
	sort(unique(as.vector(val))), "\n")
}

###	the species data block
x <- data.frame(abbr = txa, val, stringsAsFactors = FALSE)

#	attributes
#	test string length
ne <- sapply(a.txt, nchar)
if (any(ne < median(ne))) {
	a.txt[ne < median(ne)] <-
		str_pad(a.txt[ne < median(ne)], width = median(ne), side = "right")
}

h.m <- matrix(" ", ncol = length(a.txt),
	nrow = max(sapply(a.txt, nchar)))
vals <- sapply(a.txt, function (x) {
	sapply(1:nchar(x), function (y) substring(x, y, y), USE.NAMES = FALSE)
}, simplify = FALSE, USE.NAMES = FALSE)
h.m[] <- unlist(vals)
h.m <- t(h.m)

par <- str_trim(apply(h.m[, 1:(first.dot -1)], 1,
	function (x) paste(x, collapse = "")), side = "right")
val <- h.m[, first.dot: ncol(h.m)]

#	check for spaces as seperators
#	hopefully less typos as in species matrix
n.space <- apply(val, 2,
	function (x) sum(sapply(x, function (y) y == " ")) )
#	and omit
#	needs a second condition because there
#	might be a blank in each column
if (any(n.space == 0) | any(n.space == nrow(val))) {
	val <- val[, n.space != nrow(val)]
}

#	for sanity
if (dim(val)[2] != (dim(x)[2] - 1)) {
	stop("\nplease check your header data for misplaced characters")
}

#	header blocking variable
for (i in 1:length(par)) {	
	if (par[i] == "") {
		par[i] <- last
	} else {
		last <- par[i]
	}
}

### the header attributes	
y <- data.frame(par, val, stringsAsFactors = FALSE)
attr <- vector("list", length = length(unique(par)))
names(attr) <- unique(par)

for (i in par) {
	#	i = unique(par)[3]
	tmp <- str_trim(
		apply(y[y[, 1] == i, -1], 2, function (x) {
			paste(x, collapse = "")			
		}))
	attr[[i]] <- type.convert(tmp)	
}


#	finally assign abbr to rownames and turn into matrix
#	test if rownames can be assigned
if (length(unique(x[, 1])) != nrow(x) & !has.layers & !species.only) {
	warning("\nspecies are not unique.",
		" is the data structured in layers?",
		"\nreturn vector of species instead of matrix")
	x <- (txa)	
} else {
	if (species.only) {
		x <- (txa)		
	} else {
		#	paste layers
		if (has.layers) {
			rownames(x) <- paste(x[, 1], l, sep = "@")	
		} else {
			rownames(x) <- x[, 1]
		}
		x <- x[, -1]
		x <- as.matrix(x)

		#	assign header as attribute
		#	assign plot ids
		if (!missing(colnames)) {
			dimnames(x)[[2]] <- attr[[colnames]]
			attributes(x) <- c(attributes(x), attr)
		} else {
			dimnames(x)[[2]] <- NULL
			attributes(x) <- c(attributes(x), attr)
		}

		class(x) <- c("matrix", "VegsoupVerbatim")
	}
}

return(x)

}

print.VegsoupVerbatim <- function (x) {
	print(as.data.frame(x))
}

#	function to append to class VegsoupVerbatim
verbatim.append <- function (x, file, abundance = "+") {

if (!inherits(x, "VegsoupVerbatim")) {
	stop("plaese supply an object of class VegsoupVerbatim")	
}
if (missing(file)) {
	stop("plaese supply a path to a file")
}
if (!missing(abundance)) {
	stopifnot(length(abundance) == 1)
	abundance <- as.character(abundance)
}

#	save attributes
attr <- attributes(x)

con <- file(file)
	txt <- readLines(con)
close(con)

txt <- sapply(txt, function (x) {
	strsplit(x, ":", fixed = TRUE)
}, USE.NAMES = FALSE)	


rn <- str_trim(unlist(lapply(txt, "[[", 1)))
xx <- gsub("[[:blank:]]", "", unlist(lapply(txt, "[[", 2)))
xx <- strsplit(xx, ",")
names(xx) <- rn

y <- matrix(".", nrow = length(xx), ncol = ncol(x))
colnames(y) <- colnames(x)
rownames(y) <- rn

for (i in 1:length(xx)) {
	#	i = 1
	ii <- match(names(xx)[i], rownames(y))
	stopifnot(length(ii) == 1)
	jj <- match(xx[[i]], colnames(y))

	y[ii, jj] <- "+"
}

test <- intersect(rownames(x), rownames(y))
if (length(test) != 0) {
	stop("some species in file are already prsent in object x: ", test)
}
x <- rbind(x, y)

class(x) <- c("matrix", "VegsoupVerbatim")
attributes(x) <- c(attributes(x), attr[-c(1:2)])
return(x)	
}

#	reverse geocoding
#	readLines(url("http://maps.google.com/maps/geo?q=1600+Straßham+Wilhering+CA&output=csv&key=abcdefg"), n=1, warn=FALSE)
