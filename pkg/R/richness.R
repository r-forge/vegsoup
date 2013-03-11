#	Species richness of data set
setGeneric("richness",
	function (obj, ...)
		standardGeneric("richness")
)
setMethod("richness",
    signature(obj = "Vegsoup"),
	function (obj, choice = c("dataset", "sample"), ...) {
		#	obj = sub
		CHOICES <- c("dataset", "sample")
		if (missing(choice)) {
			choice <- "dataset"
		}
		choice <- CHOICES[pmatch(choice, CHOICES)]
    	if (is.na(choice)) 
        	stop("invalid choice")
		if (choice == -1) 
        	stop("ambiguous choice")
		switch(choice, "dataset" = {
			res <- length(unique(split.abbr(obj)$abbr))
		}, "sample" = {
			#	slow but reliable	
			res <- rowSums(Layers(obj, aggregate = "layer", verbose = FALSE))
			#	ensure order
			res <- res[match(rownames(obj), names(res))]
		})
		return(res)
	}
)

setMethod("richness",
    signature(obj = "VegsoupPartition"),
	function (obj, choice = c("dataset", "sample", "partition"), ...) {
		#	obj = sub
		CHOICES <- c("dataset", "sample", "partition")
		if (missing(choice)) choice <- "dataset"
		choice <- CHOICES[pmatch(choice, CHOICES)]
		if (is.na(choice)) {
			choice <- "dataset"
		}		
		switch(choice, "dataset" = {
			res <- length(unique(split.abbr(obj)$abbr))
		}, "sample" = {
			#	slow but reliable	
			res <- rowSums(Layers(obj, aggregate = "layer", verbose = FALSE))
			#	ensure order
			res <- res[match(rownames(obj), names(res))]			
		}, "partition" = {
		#	also not very fast?	
			res <- as.logical(Layers(obj, aggregate = "layer", verbose = FALSE))
			res <- sapply(1:getK(obj),
				function (x) {
					tmp <- res[Partitioning(obj) == x, ]
					sum(colSums(tmp) > 0)
				}
			)
			names(res) <- 1:getK(obj)
		})
		return(res)
	}
)