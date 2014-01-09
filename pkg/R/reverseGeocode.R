#	implement route option in method!

".reverseGeocode" <- function (lnglat, pm = 100, route = FALSE, ...) {
	#	Suggests:
	require(ggmap)
	#	Suggests
	require(geonames)	
	
	lnglat <- as.numeric(as.character(lnglat))
	r <- revgeocode(lnglat, output = c("more"))
	r <- as.data.frame(as.matrix(r), stringsAsFactors = FALSE)
	
	r1 <- r[, grep("country", names(r))]
	r2 <- paste(unique(as.character(r[, rev(grep("administrative", names(r)))])), collapse = ", ")
	#	eithetr one should be returned?
	r31 <- unlist(r[, grep("locality", names(r))])
	r32 <- unlist(r[, grep("route", names(r))])
	#	r32 mostly not meaningful
	if (is.null(r31)) {
		if (!is.null(r32)) r3 <- if (route) r32 else NA else r3 <- NA
	} else {
		r3 <- r31
	}
	locality <- paste(r1, ", ",	r2,
		ifelse(!is.na(r3), paste(", ", r3, sep = ""), ""),
		collapse = ", ", sep = "")
	options(warn = -1)
	masl <- GNsrtm3(lat = lnglat[2], lng = lnglat[1])[[1]]
	options(warn = 0)
	ew <- paste(format(abs(lnglat[1]), nsmall = 6), ifelse(lnglat[1] < 0, "W", "E"), sep = "")
	ns <- paste(format(abs(lnglat[2]), nsmall = 6), ifelse(lnglat[2] < 0, "S", "N"), sep = "")
	pm <- paste("\u00B1", pm, "m", sep = "") # PLUS-MINUS SIGN
	al <- paste(ifelse(masl == -32768, "N/A", c(round(masl/10) * 10)), "masl")
	coordinates <- paste(al, ", ", ns, ", ", ew, ", ", pm, sep = "") 
	
	return(list(coordinates = coordinates, locality = locality))
}

#if(!isGeneric("reverseGeocode")) {
setGeneric("reverseGeocode",
	function (x, ...)
		standardGeneric("reverseGeocode")
)
#}

setMethod("reverseGeocode",
    signature(x = "Vegsoup"),
    function (x, ...) {
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

setMethod("reverseGeocode",
    signature(x = "SpatialPointsDataFrame"),
    function (x, ...) {
    	m <- coordinates(x)[, 1:2]
    	p <- sapply(c("accuracy", "precision"),
    		function (y) agrep(y, names(x)))
    	l <- sapply(p, length) > 0
    	if (any(l)) {
    		p <- unlist(p[which(l)])
    	} else {
    		message("variables accuracy or precision not found")
    	}
    	if (length(p) == 1) {
    		p <- slot(x, "data")[, p]
    		if (!is.numeric(p)) {
    			p <- as.numeric(p)
    			if (any(is.na(p))) {
    				na <- 20
    				p[is.na(p)] <- na
    				message("replace NA with ", na)
    			}
    		}
    	} else {
    		message("multiple matches for variables accuracy or precision")
    		p <- rep(NA, nrow(x))
    	}
    	ll <- apply(cbind(m, p), 1, function (x) {
    		.reverseGeocode(x[1:2], x[3])	
    	}
    	)
		coordinate.string <- sapply(ll, "[[", 1)
		locality <- sapply(ll, "[[", 2)
		r <- as.matrix(cbind(coordinate.string, locality))
    return(r)
    }
)
  