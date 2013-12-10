".reverseGeocode" <- function (lnglat, pm = 100) {
	require(ggmap)
	require(geonames)	
	#lnglat <- c(13.297577, 47.612644)
	#pm <- 10	# precision
	lnglat <- as.numeric(as.character(lnglat))
	df <- revgeocode(lnglat, output = c("more"))
	df <- as.data.frame(as.matrix(df), stringsAsFactors = FALSE)
	
	locality <- paste(
		df[, grep("country", names(df))], ", ",
		paste(unique(as.character(df[, rev(grep("administrative", names(df)))])),
			collapse = ", "), ", ",
		df[, grep("locality", names(df))],# might be missing
		collapse = ", ", sep = "")
	options(warn = -1)
	masl <- GNsrtm3(lat = lnglat[2], lng = lnglat[1])[[1]]
	options(warn = 0)
	ew <- paste(format(abs(lnglat[1]), nsmall = 6), ifelse(lnglat[1] < 0, "W", "E"), sep = "")
	ns <- paste(format(abs(lnglat[2]), nsmall = 6), ifelse(lnglat[2] < 0, "S", "N"), sep = "")
	pm <- paste("±", pm, "m", sep = "")
	al <- paste(ifelse(masl == -32768, "N/A", c(round(masl/10) * 10)), "masl")
	coordinates <- paste(al, ", ", ns, ", ", ew, ", ", pm, sep = "") 
	
	return(list(coordinates = coordinates, locality = locality))
}

".compassDirection"  <- function (x) {
	res <- cut(18, breaks = c(0, seq(22.5/2, 360, by = 22.5)),
	labels = c("N","NNE","NE","ENE","E","ESE","SE","SSE",
	"S","SSW","SW","WSW","W","WNW","NW","NNW"))
	return(as.character(res))
}

#require(vegsoup)
#	calculate prediction accuracy statistics for two partitionings
#if(!isGeneric("reverseGeocode")) {
setGeneric("reverseGeocode",
	function (x, ...)
		standardGeneric("reverseGeocode")
)
#}

setMethod("reverseGeocode",
    signature(x = "Vegsoup"),
    function (x, pm) {
    	m <- coordinates(x)[, 1:2]
    	p <- grep("horizontal.precision", names(x))
    	if (length(p) == 1) {
    		p <- Sites(x)[,p]
    		if (!is.numeric(p)) {
    			p <- as.numeric(p)
    			if (any(is.na(p))) {
    				na <- 20
    				p[is.na(p)] <- na
    				message("replace NA with ", na)
    			}
    		}
    	} else {
    		message("variable horizontal.precision not found")
    	}
    	ll <- apply(cbind(m, p), 1, function (x) {
    		.reverseGeocode(x[1:2], x[3])	
    	}
    	)
		x$coordinate.string <- sapply(ll, "[[", 1)
		x$locality <- sapply(ll, "[[", 2)
    return(x)
    }
)  