compass  <- function (x, fine = TRUE) {
	
	if (fine) {
		b <- c(0, seq(22.5/2, 360, by = 22.5))
		l <- c("N","NNE","NE","ENE","E","ESE","SE","SSE",
				"S","SSW","SW","WSW","W","WNW","NW","NNW")
	} else {
	    b <- c(0, seq(45/2, 360, by = 45))
    	l <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    }
    
	if (missing(x)) {
		r <- list(breaks = b, labels = l)
	} else {
		stopifnot(is.numeric(x))
		stopifnot(!any(x < 0))	
		r <- cut(x,	breaks = b, labels = l)
		r[is.na(r)] <- "N"
		r <- as.character(r)
	}
	
	return(r)
}

singletons <- function (obj) {
	any(table(partitioning(obj)) == 1)
}

singleton <- function (obj) {
	as.vector(which(table(partitioning(obj)) == 1))
}

