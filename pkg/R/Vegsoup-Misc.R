QueryTaxonomy <- function (x, y, file.x, file.y, csv2 = TRUE, verbose = FALSE) {

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
	taxonomy <- ifelse(csv2,
		read.csv2(file.y, stringsAsFactors = FALSE, check.names = FALSE),
		read.csv(file.y, stringsAsFactors = FALSE, check.names = FALSE))	
}

if (which(sel) == 3) {
	species <- ifelse(csv2,
		read.csv2(file.x, stringsAsFactors = FALSE, check.names = FALSE),
		read.csv(file.x, stringsAsFactors = FALSE, check.names = FALSE))
	taxonomy <- y
}

if (which(sel) == 4) {
	species <- ifelse(csv2,
		read.csv2(file.x, stringsAsFactors = FALSE, check.names = FALSE),
		read.csv(file.x, stringsAsFactors = FALSE, check.names = FALSE)
		)
	taxonomy <- ifelse(csv2,
		read.csv2(file.y, stringsAsFactors = FALSE, check.names = FALSE),
		read.csv(file.y, stringsAsFactors = FALSE, check.names = FALSE))
}

#	keep only two columns
taxonomy <- taxonomy[c("abbr", "taxon")]

#	check unique abbrevations
rownames(taxonomy) <- taxonomy$abbr

res <- taxonomy[as.character(unique(species$abbr)), ]
if (any(is.na(res[, 1]))) {
	test <- as.character(unique(species$abbr))[is.na(res[,1])]
	cat("not found the following abbrevation(s) in supplied reference list\n")
	cat(test, "\n")
	stop("Please review your data")
}
return(res)
}

#	reshape tables where layers are in seperate columns

ReshapeMultiCoverColumns <- function (filename) {

res <- read.csv2(filename, colClasses = "character")

res <- rbind(
	cbind("hl", as.matrix(res[,c(1,2,3)])),
	cbind("sl", as.matrix(res[,c(1,2,4)])),
	cbind("tl", as.matrix(res[,c(1,2,5)])),
	cbind("ml", as.matrix(res[,c(1,2,6)]))
)


res <- as.data.frame(res,
	stringsAsFactors = FALSE)
res <- res[,c(2,3,1,4)]
names(res) <- c("plot", "abbr", "layer", "cov")

res <- res[res$cov != "0",]
res <- res[res$cov != "",]

}

#	plot.column, elevation.column are converted using to.lower

Shp2SitesLong <- function (dsn, layer, plot.column, elevation.column, round = TRUE) {

if (missing(plot.column)) {
	stop ("please supply column name indicating plot ids in dbf file")
}

require(rgdal)
pt <- readOGR(dsn, layer)
names(pt) <- tolower(names(pt))
pt <- spTransform(pt, CRS("+init=epsg:4326"))

if (missing(elevation.column)) {
	df <- data.frame(coordinates(pt),
		as.character(pt@data[,names(pt) == tolower(plot.column)]),
		stringsAsFactors = FALSE)
} else {
	df <- data.frame(coordinates(pt),
		as.numeric(as.character(pt@data[,names(pt) == tolower(elevation.column)])),
		as.character(pt@data[,names(pt) == tolower(plot.column)]),
		stringsAsFactors = FALSE)
}

if (dim(coordinates(pt))[2] == 2 & missing(elevation.column)) {
	
	names(df)[1:3] <- c("longitude", "latitude", "plot")
	res <- data.frame(as.character(df$plot), stack(df),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	names(res) <- c("plot", "variable", "value")
	res$value[res$variable == "longitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "latitude"] <-
		round(res$value[res$variable == "longitude"], 6)

}

if (dim(coordinates(pt))[2] == 2 & !missing(elevation.column)) {
	names(df)[1:4] <- c("longitude", "latitude", "altitude", "plot")

	res <- data.frame(as.character(df$plot),
		stack(df, select = 1:3),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	res[,3] <- as.numeric(as.character(res[,3]))
	names(res) <- c("plot", "variable", "value")
	res$value[res$variable == "longitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "latitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "altitude"] <-
		round(res$value[res$variable == "altitude"], 0)

}

if (dim(coordinates(pt))[2] == 3) {
	names(df)[1:4] <- c("longitude", "latitude", "altitude", "plot")
	res <- data.frame(as.character(df$plot), stack(df, select = 1:3),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	names(res) <- c("plot", "variable", "value")
	res$value[res$variable == "longitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "latitude"] <-
		round(res$value[res$variable == "latitude"], 6)
	res$value[res$variable == "altitude"] <-
		round(res$value[res$variable == "altitude"], 0)

}

return(invisible(res))

}

#	stack sites data frame to match database structure

SitesWide2SitesLong <- function (x, file, csv2 = TRUE, verbose = FALSE) {

if (missing(x) & missing(file)) {
	stop("please supply either a data frmae or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		if (csv2) {
			x <- read.csv2(file,
				stringsAsFactors = FALSE, check.names = FALSE)
		} else {
			x <- read.csv2(file,
				stringsAsFactors = FALSE, check.names = FALSE)
		}
	}
} else {
	if (is.data.frame(x) & missing(file)) {
		x <- x
		} else {
			stop("please supply a data.frame")	
	}
}


#	drop any columns coded as factor to use stack()
res <- as.data.frame(as.matrix(x), stringsAsFactors = FALSE)

res.stack <- stack(res, stringsAsFactors = FALSE)
res.stack[,1] <- as.character(res.stack[,1])
res.stack[,2] <- as.character(res.stack[,2])
plot <- res.stack[res.stack$ind == "plot",]$values
plot <- rep(plot, (nrow(res.stack)/length(plot))- 1)
res.stack <- res.stack[!res.stack$ind == "plot",]
res.stack <- data.frame(plot,
	variable = res.stack[,2],
	value = res.stack[,1])
res.stack <- res.stack[order(res.stack$plot),]
res.stack[is.na(res.stack)] <- ""
rownames(res.stack) <- 1:nrow(res.stack)
res <- res.stack
res$value <- as.character(res$value)
res$plot <- as.character(res$plot)
res$variable <- as.character(res$variable)

if (verbose) {
	print(unique(res$variable))
}	

return(invisible(res))
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
	stop("please supply either a data frmae or a csv file")	
}

if (!missing(file)) {
	if (is.character(file)) {
		if (csv2) {
			x <- read.csv2(file,
				stringsAsFactors = FALSE, check.names = FALSE)
		} else {
			x <- read.csv2(file,
				stringsAsFactors = FALSE, check.names = FALSE)
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

if (verbose) {
	print(table(res$cov))
	print(table(res$layer))
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