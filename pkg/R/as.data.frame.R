setAs(from = "Vegsoup", to = "data.frame",
	def = function (from) {
		i <- rep(1:nrow(from), rle(species(from)$plot)$lengths)
	
		res <- data.frame(
			species(species(from)), #! use slot data
			sites(from)[i, ],
			coordinates(from)[i, ],
			taxonomy(taxonomy(from))[species(from)$abbr, -1, drop = FALSE]
		)
		
		return(res)
		# typeof = "character", mode = "Q"
} )

#	ensure that also base functions dispatch properly
as.data.frame.Vegsoup <- function (x, ...) as(x, "data.frame")


setAs(from = "Sites", to = "data.frame",
	def = function (from) {
	r <- reshape(sites(from), direction = "wide",
		timevar = "variable",
		idvar = "plot")
	names(r) <- gsub("value.", "", names(r), fixed = TRUE)
	#	save row names
	ii <- as.character(r$plot) # leading zeros!
	r <- r[, names(r) != "plot", drop = FALSE] 
	r <- as.data.frame(sapply(r, function (x) type.convert(x), simplify = FALSE))
	rownames(r) <- ii
#	if (!stringsAsFactors) {
#		r <- as.data.frame(as.matrix(r), stringsAsFactors = FALSE)
	return(r)
} )

#	ensure that also base functions dispatch properly
as.data.frame.Sites <- function (x, ...) as(x, "data.frame")
