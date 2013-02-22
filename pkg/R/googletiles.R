#	http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames

#	tile index from coordiantes
latlng2tile <- function(lat, lng, z){
	n <- 2 ^ z # zoom
	lat <- lat * pi / 180
	#	xtile
	x <- floor((lng + 180.0) / 360.0 * n)
	#	ytile
	y <- floor((1 - log(tan(lat) + (1 / cos(lat))) / pi) / 2.0 * n)
	return(c(x = x, y = y, z = z))
} 	

#	coordiantes from tile index
tile2latlng <- function (x, y, z, offset = 0) {
	x <- as.numeric(x) + offset
	y <- as.numeric(y) + offset
	z <- as.numeric(z)
	n <-  2 ^ z
	lng <- x / n * 360 - 180
	lat <- atan(sinh(pi * (1 - 2 * (y / n)))) * 180 / pi
	return(c(latitude = lat, longitude = lng))
}

#	coordiantes from tile index as URL style string
str2latlng <- function (xyz, offset = 0.5, sep = ",", style = c("google", "osm")) {
	if (missing(style)) {
		style <- "google"
	} else {	
		STYLES <- c("google", "osm")
		style <- match.arg(style, STYLES)
	}
	if (length(grep(sep, xyz, fixed = TRUE)) > 0 | length(xyz) > 1) {
		if (length(xyz) == 1) {
			xyz <- strsplit(xyz, sep, fixed = TRUE)[[1]]
		}
		res <- t(sapply(xyz, function (x)
			.str2latlng.single(x, offset = offset, style = style)))
		res <- colMeans(res)
	} else {
		res <- .str2latlng.single(xyz, offset = offset, style = style)
	}
	return(res)	
}

".str2latlng.single" <- function (xyz, offset, style) {
	stopifnot(is.character(xyz))
	
	if (missing(style)) {
		style <- "google"
	}
	if (style == "google") {
		stopifnot(grep("x", xyz) == 1 & grep("y", xyz) == 1 & grep("z", xyz) == 1)
		xyz <- strsplit(xyz, "&", fixed = TRUE)[[1]]
		#	remove what is not needed
		s <- grep("s=", xyz)
		if (length(s) > 0) xyz <- xyz[-s]
		xyz <- as.numeric(gsub("[[:alpha:]]", "", gsub("[[:punct:]]", "", xyz)))		
	}
	if (style == "osm") {
		#	how to test?
		xyz <- strsplit(xyz, "/", fixed = TRUE)[[1]][c(2,3,1)]
	}
		
	res <- tile2latlng(xyz[1], xyz[2], xyz[3], offset = offset)
	
	#	tile dimension in meters to calculate uncertainty
	res[3] <- floor(256 * abs(156543.034 * cos(res[1]) / (2 ^ xyz[3])))
	res[3] <- round(res[3] * sqrt(2) / 2, 0) # half diagonal
	res[4] <- xyz[3]
	names(res) <- c("latitude", "longitude", "precision", "zoom")
	return(res)
}

#	not documented, trival
whatshere2lnglat <- function (x) {
	res <- as.numeric(strsplit(x, ",")[[1]])
	names(res) <- c("latitude", "longitude")
	return(res)
}


