compass  <- function (x) {
	stopifnot(is.numeric(x))
	stopifnot(any(x < 0))
	r <- cut(x,
		breaks = c(0, seq(22.5/2, 360, by = 22.5)),
		labels = c("N","NNE","NE","ENE","E","ESE","SE","SSE",
			"S","SSW","SW","WSW","W","WNW","NW","NNW"))
	r[is.na(r)] <- "N"
	return(as.character(r))
}
