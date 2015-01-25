#if (!isGeneric("richness")) {
setGeneric("richness",
	function (obj, ...)
		standardGeneric("richness")
)
#}

setMethod("richness",
	signature(obj = "Species"),
	function (obj, choice = c("dataset", "sample")) {

		CHOICES <- c("dataset", "sample")

		#	default
		if (missing(choice)) choice <- "dataset"
		
		choice <- CHOICES[pmatch(choice, CHOICES)]
		if (is.na(choice)) stop("invalid choice", call. = FALSE)
		if (choice == -1)  stop("ambiguous choice", call. = FALSE)
						
		switch(choice, "dataset" = {
			r <- length(unique(species(obj)$abbr))
		}, "sample" = {
			r <- unique(species(obj)[c("plot", "abbr")])
			r <- rle(r$plot)
			r <- structure(r$lengths, names = r$values)
		})
		return(r)					
	}
)

#	generic is set in Species-methods.R
setMethod("richness",
	signature(obj = "Vegsoup"),
	function (obj, choice = c("dataset", "sample")) {
		if (missing(choice)) choice <- "dataset"
		r <- richness(species(obj), choice = choice) # use Species-method
		return(r)
	}
)

setMethod("richness",
	signature(obj = "VegsoupPartition"),
	function (obj, choice = c("dataset", "sample", "partition")) {
		
		CHOICES <- c("dataset", "sample", "partition")

		#	default
		if (missing(choice)) choice <- "dataset"
		
		choice <- CHOICES[pmatch(choice, CHOICES)]
		if (is.na(choice)) stop("invalid choice", call. = FALSE)
		if (choice == -1)  stop("ambiguous choice", call. = FALSE)
		
		switch(choice, "dataset" = {
			r <- richness(species(obj), choice = "dataset") # use Species-method
		}, "sample" = {
			r <- richness(species(obj), choice = "sample")  # use Species-method
		}, "partition" = {
			r <- sapply(1:getK(obj), function (x) {
				richness(partition(obj, x), "dataset")
			})
		})
		return(r)
	}
)