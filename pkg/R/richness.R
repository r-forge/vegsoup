#	Species richness of data set
setGeneric("richness",
	function (obj, ...)
		standardGeneric("richness")
)
setMethod("richness",
    signature(obj = "Vegsoup"),
	function (obj, choice = c("dataset", "sample"), ...) {
		#	obj = sub
		CHOICE <- c("dataset", "sample")
		if (missing(choice)) choice <- "dataset"
		choice <- CHOICE[pmatch(choice, CHOICE)]
		if (is.na(choice)) {
			choice <- "dataset"
		}		
		switch(choice, "dataset" = {
			res <- length(unique(DecomposeNames(obj)$abbr))
		}, "sample" = {
		#	slow but reliable	
			res <- rowSums(Layers(obj, aggregate = "layer", verbose = FALSE))
		})		

		return(res)
	}
)