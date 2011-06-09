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