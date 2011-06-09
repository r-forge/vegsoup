Shp2SitesLong <- function (dsn, layer, plot.column, round = TRUE) {

require(rgdal)
pt <- readOGR("/Users/roli/Desktop/va.rar Folder", "va")


pt <- spTransform(pt, CRS("+init=epsg:4326"))

df <- data.frame(coordinates(pt),
	as.character(pt@data[names(pt) == plot.column,]))

if (dim(coordinates(pt))[2] == 2) {#
	names(df)[1:3] <- c("longitude", "latitude", "plot")
	res <- data.frame(rep(as.character(df$plot), 3), stack(df),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	names(res) <- c("plot", "variable", "value")
	res$value[res$variable == "longitude"] <-
		round(res$value[res$variable == "longitude"], 6)
	res$value[res$variable == "latitude"] <-
		round(res$value[res$variable == "longitude"], 6)

}
if (dim(coordinates(pt))[2] == 3) {#
	names(df)[1:4] <- c("longitude", "latitude", "altitude", "plot")
	res <- data.frame(as.character(df$plot), stack(df),
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

df <- Shp2SitesLong(
	dsn = "/Users/roli/Documents/hohewand/dta/shp/pt_plots",
	layer = "pt_plots",
	plot.column = "PLOT")
write.csv2(df, "~/foo.csv", row.names = FALSE)