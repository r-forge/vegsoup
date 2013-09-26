#if (!isGeneric("as.data.frame")) {
#setGeneric("as.data.frame",
#	function (x, ...)
#	standardGeneric("as.data.frame"))
#}
#setMethod("as.data.frame",
#   signature(x = "Vegsoup"),
#    function (x, ...) {  		
#    	return(Sites(x))
#    }    	    
#)

setAs(from = "Vegsoup", to = "data.frame",
	def = function (from) {
		#from = dta	
		
		replicates <- rep(1:nrow(from), rle(Species(from)$plot)$lengths)
	
		res <- data.frame(
		    Species(from),
			Sites(from)[replicates, ],
			coordinates(from)[replicates, ],
			Taxonomy(from)[Species(from)$abbr, -1, drop = FALSE]
		)
		
		return(res)
		# typeof = "character", mode = "Q"
	}
)

#	ensure that also base functions dispatch properly
as.data.frame.Vegsoup <- function (x, ...) as(x, "data.frame")