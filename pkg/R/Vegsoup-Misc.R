#	query and copy taxonomy.csv file matching species.csv
#	x: filename species.csv
#	y: filename taxonomic reference list
QueryTaxonomy <- function (x, y) {

species <- read.csv2(x)
taxonomy <- read.csv2(y)
taxonomy <- taxonomy[c("abbr", "taxon")]
#	this checks for unique abbrevations
rownames(taxonomy) <- taxonomy$abbr

res <- taxonomy[as.character(unique(species$abbr)), ]
if (any(is.na(res[, 1]))) {
	test <- as.character(unique(species$abbr))[is.na(res[,1])]
	warning("not found the following abbrevation(s) in supplied reference list")
	print(test)
}
return(res)

}

#y = "/Users/roli/Dropbox/vegbase standards/austrian standard list 2008/austrian standard list 2008.csv"	
#x = "/Users/roli/Documents/vegsoup/testing/amadeus dta/species.csv"	

taxonomy <- QueryTaxonomy(x, y)

#	reshape tables where layers are in seperate columns

ReshapeMultiCoverColumns <- function (filename) {

res <- read.csv2(filename, colClasses = "character")


res <- rbind(
	cbind("hl", as.matrix(res[,c(1,2,3)])),
	cbind("sl", as.matrix(res[,c(1,2,4)])),
	cbind("tl", as.matrix(res[,c(1,2,5)])),
	cbind("ml", as.matrix(res[,c(1,2,6)])))	


res <- as.data.frame(res,
	stringsAsFactors = FALSE)
res <- res[,c(2,3,1,4)]
names(res) <- c("plot", "abbr", "layer", "cov")

res <- res[res$cov != "0",]
res <- res[res$cov != "",]


}
#res <- ReshapeMultiCoverColumns("/Users/roli/Desktop/db/relevees/species.csv")
#write.csv2(res, "foo.csv")

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
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "altitude"] <-
		round(res$value[res$variable == "altitude"], 0)

}

return(invisible(res))

}

#df <- Shp2SitesLong(
#	dsn = "/Users/roli/Desktop/rek.rar Folder",
#	layer = "va",
#	plot.column = "comment")
#write.csv2(df, "~/foo.csv", row.names = FALSE)

#	stack sites data frame to match database structure

SitesWide2SitesLong <- function (filename) {

sites <- read.csv2(filename, colClasses = "character")

sites.stack <- stack(sites)
sites.stack[,1] <- as.character(sites.stack[,1])
sites.stack[,2] <- as.character(sites.stack[,2])
plot <- sites.stack[sites.stack$ind == "plot",]$values
plot <- rep(plot, (nrow(sites.stack)/length(plot))- 1)
sites.stack <- sites.stack[!sites.stack$ind == "plot",]
sites.stack <- data.frame(plot,
	variable = sites.stack[,2],
	value = sites.stack[,1])
sites.stack <- sites.stack[order(sites.stack$plot),]
sites.stack[is.na(sites.stack)] <- ""
rownames(sites.stack) <- 1:nrow(sites.stack)
sites <- sites.stack
return(sites)
}

#sites <- stack.sites("sites.csv")
#write.csv2(sites, "sites2.csv", row.names = FALSE)

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