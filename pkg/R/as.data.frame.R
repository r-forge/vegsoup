#as.data.frame(x, row.names = NULL, optional = FALSE, ..., 
#              stringsAsFactors = default.stringsAsFactors())

#if (!isGeneric("as.data.frame")) {
setGeneric("as.data.frame",
	function (x, ...)
	standardGeneric("as.data.frame"))
#}
setMethod("as.data.frame",
    signature(x = "Vegsoup"),
    function (x, ...) {  		
    	return(Sites(x))
    }    	    
)
setAs(from = "Vegsoup", to = "data.frame",
	def = function (from) {
		as.data.frame(from)
		# typeof = "character", mode = "Q"
	}
)

#	ensure that also base functions dispatch properly
as.data.frame.Vegsoup <- function (x, ...) as(x, "data.frame")