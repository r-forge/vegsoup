#	function to import monospaced commuity tables
read.verbatim <- function (file, colnames, layers, replace = c("|", "-", "–", "_"), species.only = FALSE, verbose = TRUE) {

require(stringr)

if (missing(file)) {
	stop("plaese supply a path to a file")
}

if (!missing(layers)) {
	if (!is.list(layers) | !is.vector(layers)) {
		stop("layers must be a list or character vector")
	} else {
		if (is.list(layers)) {
			stopifnot(length(names(layers)) == length(layers))
			l <- rep(names(layers), lapply(layers, function (x) diff(x) + 1))	
		} else {
			l <- layers
		}
		has.layers <- TRUE		
	}
} else {
	has.layers <- FALSE	
}

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
#	for example, lines consisting of only spaces
ul <- el < median(el) & el != 0
#	hook keywords
ul[hks] <- FALSE
el <- el == 0
el[ul] <- TRUE

#	trim over long lines
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
test <- sapply(t.txt, nchar)
if (length(unique(test)) > 1) {
	stop("length of characters",
		" is not the same for all lines!",
		call. = FALSE)
}
test <- sapply(a.txt, nchar)
if (length(unique(test)) > 1) {
	stop("length of characters",
		" is not the same for all lines!",
		"\n please inspect line(s) ", which(test != median(test)),
		" in HEADER",
		call. = FALSE)
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
	warning("some mono type character columns deviate from the expected pattern\n")
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
	stop("\nplease review your data and apply necessary changes")
} else {
	if (verbose) {
		cat("\nfound no obvious errors in species data block",
			"\nskip",
			 ifelse(is.na(table(n.space)[2]), 0, table(n.space)[2]),
			 "columns of blanks",
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
	attr[[i]] <- type.convert(tmp) # this will drop leading zeros!	
}

#	finally assign abbr to rownames and turn into matrix
#	test if rownames can be assigned
if (length(unique(x[, 1])) != nrow(x) & !has.layers & !species.only) {
	warning("\nspecies are not unique.",
		" is the data structured in layers?",
		"\nreturn vector of species instead of matrix")
	x <- txa	
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
read.verbatim.append <- function (x, file, mode = c("plots", "species", "layers"), collapse = ",", abundance) {

require(stringr)

if (!inherits(x, "VegsoupVerbatim")) {
	stop("plaese supply an object of class VegsoupVerbatim")	
}
if (missing(file)) {
	stop("please supply a path to a file")
}
if (missing(mode)) {
	mode = "plots"
	warning("missing mode, but set mode to", mode, call. = FALSE)	
} else {
	MODES <- c("plots", "species", "layers")
	mode <- match.arg(mode, MODES)
	if (mode == "layers") {
		stop("mode \"layers\" not yet implemented!", call. = FALSE)
	}
}
if (!missing(abundance)) {
	stopifnot(length(abundance) == 1)
	if (mode == 1) {
		abundance <- as.character(abundance)	
	}
	if (mode == 2) {
		abundance <- as.logical(abundance)	
	}
} else {
	if (mode == 1) {
		abundance = "+"
	}
	if (mode == 2) {
		if (abundance == FALSE) {
			abundance = "+"
		}
	}			
}

#	save attributes
attr <- attributes(x)

txt <- readLines(file.path(file))

txt <- sapply(txt, function (x) {
		strsplit(x, ":", fixed = TRUE)
	}, USE.NAMES = FALSE)

#	plain and simple uses dummy abundance
if (mode == "species") {
	rn <- str_trim(unlist(lapply(txt, "[[", 1)))
	xx <- gsub("[[:blank:]]", "", unlist(lapply(txt, "[[", 2)))
	xx <- strsplit(xx, collapse, fixed = TRUE)
	names(xx) <- rn	
	y <- matrix(".", nrow = length(xx), ncol = ncol(x))
	colnames(y) <- colnames(x)
	rownames(y) <- rn

	for (i in 1:length(xx)) {
		#	i = 1
		ii <- match(names(xx)[i], rownames(y))
		stopifnot(length(ii) == 1)
		jj <- match(xx[[i]], colnames(y))

		y[ii, jj] <- abundance
	}
}

#	more structred, uses given abundance, but no layers
if (mode == "plots") {
	cn <- str_trim(unlist(lapply(txt, "[[", 1)))
	xx <- str_trim(unlist(lapply(txt, "[[", 2)))
	xx <- sapply(strsplit(xx, collapse, fixed = TRUE), str_trim)
	tmp <- vector("list", length = length(xx))
	names(tmp) <- type.convert(cn) # handle leading zeros as in read.verbatim!

	for (i in 1:length(xx)) {
		#	i = 26
		ll <- sapply(xx[[i]], nchar)
		tmp[[i]] <- list(
			str_trim(substring(xx[[i]], 1, ll - 1)),
			substring(xx[[i]], ll, ll))
	}
	
	rn <- unique(unlist(lapply(tmp, "[[", 1)))
	y <- matrix(".", nrow = length(rn), ncol = ncol(x))
	colnames(y) <- colnames(x)
	rownames(y) <- rn
	
	for (i in 1:length(tmp)) {
		#	i = 27
		for (j in seq(along = tmp[[ i ]][[1]])) {
			#	j = 1
			ii <- match(tmp[[ i ]][[1]][ j ], rownames(y))
			jj <- match(names(tmp)[[ i ]], colnames(y))
			y[ii, jj] <- ifelse(abundance == TRUE, tmp[[ i ]][[2]][j], abundance)
		} 
	}		
}	

test <- intersect(rownames(x), rownames(y))
if (length(test) != 0) {
	stop("some species in file are already present in object x: ", test)
}
x <- rbind(x, y)

class(x) <- c("matrix", "VegsoupVerbatim")
attributes(x) <- c(attributes(x), attr[-c(1:2)])

return(x)	
}
