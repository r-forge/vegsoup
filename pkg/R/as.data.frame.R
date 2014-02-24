setAs(from = "Vegsoup", to = "data.frame",
	def = function (from) {
		#from = dta	
		
		replicates <- rep(1:nrow(from), rle(species(from)$plot)$lengths)
	
		res <- data.frame(
		    species(species(from)), #! use slot data
			Sites(from)[replicates, ],
			coordinates(from)[replicates, ],
			Taxonomy(from)[species(from)$abbr, -1, drop = FALSE]
		)
		
		return(res)
		# typeof = "character", mode = "Q"
	}
)

#	ensure that also base functions dispatch properly
as.data.frame.Vegsoup <- function (x, ...) as(x, "data.frame")