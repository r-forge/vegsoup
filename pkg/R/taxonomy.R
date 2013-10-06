#	get or set taxonomy (traits) data frame
setGeneric("Taxonomy",
	function (obj)
		standardGeneric("Taxonomy")
)
setGeneric("Taxonomy<-", function (obj, value)
	standardGeneric("Taxonomy<-")
)	
setMethod("Taxonomy",
    signature(obj = "Vegsoup"),
    function (obj) obj@taxonomy
)

# Taxonomy<-
setReplaceMethod("Taxonomy",
	signature(obj = "Vegsoup", value = "SpeciesTaxonomy"),
	function (obj, value) {
		#	to do: needs checking against Sites(obj) and Spatial*(obj)
#		obj@taxonomy <- value
		warning("method not implemented yet")		
		return(obj)		
	}
)
