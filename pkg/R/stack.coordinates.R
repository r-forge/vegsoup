#	read OGR data source
stack.coordinates <- function (dsn, layer, schema, round = TRUE, verbose = TRUE, ...) {

require(rgdal)

pt <- ogrInfo(dsn, layer)
withz <- pt$with_z

if (missing(schema)) {
	print(pt)
	stop("please supply a column name in OGR data source indicating plot ids")

}
	
#	check column names with ogrInfo
pt <- ogrInfo(dsn, layer)
withz <- ifelse(pt$with_z == 0, FALSE, TRUE)

pt.names <- pt$iteminfo$name

test <- match(schema, pt.names)

print(test)

if (any(is.na(test))) {
	cat("\nogrinfo returns\n")
	print(pt)
	cat("you supplied schema: ", schema)
	cat("\nfound:",
		ifelse(length(pt.names[test[!is.na(test)]]) == 0,
			"... nothing?",
			paste0(pt.names[test[!is.na(test)]], collapse = " ")))
	cat("\n")
	stop("\nif specified both elements have to match")		
}

pt <- readOGR(dsn, layer, ...)

pt <- rgdal::spTransform(pt, CRS("+init=epsg:4326"))

#	can be simplified!
if (!withz & length(schema) == 1) {
	df <- data.frame(
		coordinates(pt),
		plot = as.character(pt@data[, names(pt) == schema[1]]),
		stringsAsFactors = FALSE)
} else {
	if (!withz & length(schema) == 2) {
		df <- data.frame(
			coordinates(pt),
			elevation = as.numeric(as.character(pt@data[,names(pt) == schema[2]])),
			plot = as.character(pt@data[, names(pt) == schema[1]]),
			stringsAsFactors = FALSE)
	} else {
		if (withz & length(schema) == 1) {
			df <- data.frame(
				coordinates(pt)[, 1:2],
				elevation = coordinates(pt)[, 3],
				plot = as.character(pt@data[, names(pt) == schema[1]]),
				stringsAsFactors = FALSE)		
		} else { # withz & length(schema) == 2
			warning("OGR data source supports 3D",
				" but use attribute \"", schema[2], "\" to obtain heights from attributes") 
			df <- data.frame(
				coordinates(pt),
				elevation = as.numeric(as.character(pt@data[,names(pt) == schema[2]])),
				plot = as.character(pt@data[, names(pt) == schema[1]]),
				stringsAsFactors = FALSE)		
		}		
	}	
}

names(df)[1:2] <- c("longitude", "latitude")

if (!withz) {	
	res <- data.frame(
		as.character(df$plot),
		stack(df, select = 1:2),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]

}

if ((!withz & length(schema) == 2) | withz) {
	res <- data.frame(
		as.character(df$plot),
		stack(df, select = 1:3),
		stringsAsFactors = FALSE)
	res	 <- res[, c(1,3,2)]
	res[, 3] <- as.numeric(as.character(res[,3])) #	for safety
}

names(res) <- c("plot", "variable", "value")
	if (round) {
		res$value[res$variable == "longitude"] <-
			round(res$value[res$variable == "longitude"], 6)
		res$value[res$variable == "latitude"] <-
			round(res$value[res$variable == "latitude"], 6)
		res$value[res$variable == "altitude"] <-
			round(res$value[res$variable == "altitude"], 0)
	}

res <- new("Sites", data = res)	
return(invisible(res))
}
