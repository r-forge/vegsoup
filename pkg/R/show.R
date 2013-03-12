#if (!isGeneric("head")) {
setGeneric("head", function (x, ...)
	standardGeneric("head"))
#}

setMethod("head",
    signature(x = "Vegsoup"),
    function (x, n = 6L, choice, typeof, ...) {
	    if (missing(choice))
	    	choice = "species"
    	CHOICE <- c("species", "sites")
    	choice <- CHOICE[pmatch(choice, CHOICE)]
	    if (missing(typeof))
		    typeof = "logical"
	    if (missing(n))
		    n = 6L
    	if (choice == "species")
			res <- head(as.matrix(x, typeof), n, ...)
    	if (choice == "sites")
    		res <- head(Sites(x), n, ...)
    	return(res)
    }    	    
)
#if (!isGeneric("tail")) {
setGeneric("tail", function (x, ...)
	standardGeneric("tail"))
#}
#	to do: documentation
setMethod("tail",
    signature(x = "Vegsoup"),
    function (x, n = 6L, choice, typeof, ...) {
	    if (missing(choice))
	    	choice = "species"
		if (missing(typeof))
		    typeof = "logical"
	    if (missing(n))
			n = 6L
		if (choice == "species")
			res <- tail(as.matrix(x, typeof), n, ...)
    	if (choice == "sites")
    		res <- tail(Sites(x), n, ...)
    	return(res)
    }    	    
)

#	summary method
setMethod("show",
    signature(object = "Vegsoup"),
    function (object) {
			do.call("summary", list(object))
    }
)

#if (!isGeneric("summary")) {
setGeneric("summary", function(object, ...)
	standardGeneric("summary"))
#}

setMethod("summary",
    signature(object = "Vegsoup"),
    function (object, choice = c("all", "species", "sites"), ...) {

	if (missing(choice)) {
		choice <- "all"
	}
	CHOICES <- c("all", "species", "sites")
	choice <- CHOICES[pmatch(choice, CHOICES)]
	if (is.na(choice)) {
		choice <- "all"
	}
	cat("object of class", class(object), "\n")
	species.summary <- paste(
		"\n species (discarding layer duplicates): ", richness(object),
		"\n sites (sample plots): ", dim(object)[1],
		"\n matrix fill: ", round(MatrixFill(object), 0), " %",
		"\n layers: ", length(Layers(object)), 
		" (", paste(Layers(object), collapse = ", "), ")",
		"\n abundance scale: ", coverscale(object)@name,
		ifelse(is.null(decostand(object)),
			paste("\n decostand method: undefined (NULL)"),
			paste("\n decostand method: ", decostand(object))
		),		
		"\n dissimilarity: ", object@dist,	   				
		ifelse(length(object@taxonomy) > 0,
			"\n taxomomic reference: valid ",
			"\n taxomomic reference: has non matching taxa!"),
		sep = ""
	)
	if (dim(object)[1] == 1) {
		species.list <- Species(object)
		species.list$taxon <-
			Taxonomy(object)$taxon[match(species.list$abbr, Taxonomy(object)$abbr)]
		species.list <- species.list[, c(1,5,3,4)]
		species.list <- apply(species.list[, -1], 1,
			function (x) paste(x[1], " (", x[2], ") ", x[3], sep = "", collpase = ","))
	}			
	switch(choice, "all" = {
		cat(species.summary)
		if (dim(object)[1] == 1) cat("\n species list\n", species.list)
		cat("\nsites ")	
		str(Sites(object), no.list = TRUE)
	}, "species" = {
		cat(species.summary)
		if (dim(object)[1] == 1) cat("\nspecies", species.list)
	}, "sites" = {
		cat("\nsites ")
		str(Sites(object), no.list = TRUE)		
	})
	cat("\n    proj4string:\n", proj4string(object))
	cat("\n    bbox:\n"); bbox(object)		
}
)