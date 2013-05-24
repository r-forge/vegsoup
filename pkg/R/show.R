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
    	CHOICE <- c("species", "sites")
    	choice <- CHOICE[pmatch(choice, CHOICE)]
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
	cat("object of class  :", class(object))
	species.summary <- paste(
	   "\nspecies          : ",
			nrow(Taxonomy(object)), " (discarding layer/stratum duplicates)",
	   "\nmatrix fill      : ",
			round(MatrixFill(object), 0), " %",			
	   "\nlayers           : ",
			length(Layers(object)), " (", paste(Layers(object), collapse = ", "), ")",
	   "\ncoverscale       : ",
			coverscale(object)@name,
		ifelse(is.null(decostand(object)),
			paste("\ndecostand method : undefined (NULL)"),
			paste("\ndecostand method : ", decostand(object))
		),		
	   "\nvegdist          : ",
			object@dist,	   				
		ifelse(length(object@taxonomy) > 0,
	   "\nreference list   : valid ",
       "\nreference list   : non matching taxa!"),
	   "\nsites            : ",
			dim(object)[1], " (sample plots/relevees)",       
		sep = ""
	)
#	tmp <- table(sapply(Sites(object), mode))
	sites.summary <- paste(
	   "\nsite variables   :", length(names(object)))
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
	cat(sites.summary)
	}, "species" = {
		cat(species.summary)
		if (dim(object)[1] == 1) cat("\nspecies", species.list)
	}, "sites" = {
	})
	cat("\nproj4string      :", proj4string(object))
	cat("\nbbox             :",
		paste(paste(bbox(object)[1,], bbox(object)[2,]), collapse = " "),
		" (longitude latitude / min max)"
	)
}
)