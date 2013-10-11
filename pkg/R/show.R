#	show and summary methods
setMethod("show",
    signature(object = "Vegsoup"),
    function (object) {
			do.call("summary", list(object))
    }
)

setMethod("show",
    signature(object = "VegsoupOptimstride"),
    function (object) {
			summary(object)
    }
)

#	partial summary functions
.species.summary <- function (x) {
	res <- paste(
		"\nspecies          : ",
			nrow(Taxonomy(x)), " (discarding layer/stratum duplicates)",
		"\nmatrix fill      : ",
			round(fill(x), 0), " %",			
		"\nlayers           : ",
			length(Layers(x)), " (", paste(Layers(x), collapse = ", "), ")",
		"\ncoverscale       : ",
			coverscale(x)@name,
			ifelse(is.null(decostand(x)),
			paste("\ndecostand method : undefined (NULL)"),
			paste("\ndecostand method : ", decostand(x), sep = "")),		
		"\nvegdist          : ",
			x@dist,	   				
			ifelse(length(x@taxonomy) > 0,
		"\nreference list   : valid ",
	       "\nreference list   : non matching taxa!"),
		"\nsites            : ",
			dim(x)[1], " (sample plots/relevees)", sep = "")
	if (dim(x)[1] == 1) {
		tmp <- Species(x)
		tmp$taxon <-
			Taxonomy(x)$taxon[match(tmp$abbr, Taxonomy(x)$abbr)]
		tmp <- tmp[, c(1,5,3,4)]
		tmp <- tmp[order(tmp$taxon, tmp$layer), ]
		tmp <- apply(tmp[, -1], 1,
			function (x) paste(x[1], " (", x[2], ") ", x[3], sep = "", collpase = ", "))
		res <- paste(res, "\nspecies list     :", paste(tmp, collapse = ""))
	}
		
	res		
} 

.sites.summary <- function (x) {
	res <- paste(
	   "\nsite variables   :", length(names(x)))
}

.spatial.summary <- function (x) {
	res <- paste(
		"\nproj4string      :", proj4string(x),
		"\nbbox             :",
			paste(paste(bbox(x)[1,], bbox(x)[2,]), collapse = " "),
			" (lng lat / min max)")
	res
}

.partition.summary <- function (x) {
	res <- paste(
		"\n", getK(x), " partitions",
		paste(rep(" ", 17 - (nchar(getK(x)) + 11)), collapse = ""), ": ", sep = "")
	res <- paste(res, paste(as.vector(table(Partitioning(x))), collapse = " "), sep = "")
	res
}

.fidelity.summary <- function (x) {
	res <- paste(
		"\nfidelity measure :", x@method,
		ifelse(all(is.na(x@lowerCI)), 
		"\nbootstrap        : no",	
		paste("\nbootstrap        :", x@nboot, "replicates")))
	res
}

#if (!isGeneric("summary")) {
setGeneric("summary", function (object, ...)
	standardGeneric("summary"))
#}	

#	class Vegsoup
setMethod("summary",
    signature(object = "Vegsoup"),
    function (object, choice = c("all", "species", "sites"), ...) {
		if (missing(choice)) choice <- "all"
		CHOICES <- c("all", "species", "sites")
		choice <- CHOICES[pmatch(choice, CHOICES)]
		if (is.na(choice)) stop("invalid choice", call. = FALSE)
		cat("object of class  :", class(object))
		s1 <- .species.summary(object)
		s2 <- .sites.summary(object)
		s3 <- .spatial.summary(object)
		switch(choice,
			"all" = {
			cat(s1, s2, s3, "\n")
		}, "species" = {
			cat(s1, s3, "\n")
		}, "sites" = {
			cat(s2, s3, "\n")
		})
	}
)

#	class VegsoupPartition
setMethod("summary",
    signature(object = "VegsoupPartition"),
	function (object, choice = c("all", "species", "sites", "partition"), ...) {
		if (missing(choice)) choice <- "all"
		CHOICES <- c("all", "species", "sites", "partition")
		choice <- CHOICES[pmatch(choice, CHOICES)]
		if (is.na(choice)) stop("invalid choice", call. = FALSE)
    	if (choice == -1) stop("ambiguous choice", call. = FALSE)
		s1 <- .species.summary(object)
		s2 <- .sites.summary(object)
		s3 <- .spatial.summary(object)
		s4 <- .partition.summary(object)        	
		cat("object of class  :", class(object))		
		switch(choice,
			"all" = {
			cat(s1, s2, s3, s4, "\n")			
		}, "species" = {
			cat(s1, s3, s4, "\n")
		}, "sites" = {
			cat(s2, s3, s4)
		}, "partition" = {
			cat(s4, "\n")
		})	
	}
)

#	class VegsoupPartitionFidelity
setMethod("summary",
    signature(object = "VegsoupPartitionFidelity"),
	function (object, choice = c("all", "species", "sites", "partition", "fidelity"), ...) {
		if (missing(choice)) choice <- "all"
		CHOICES <- c("all", "species", "sites", "partition", "fidelity")
		choice <- CHOICES[pmatch(choice, CHOICES)]
		if (is.na(choice)) stop("invalid choice", call. = FALSE)
    	if (choice == -1) stop("ambiguous choice", call. = FALSE)
		s1 <- .species.summary(object)
		s2 <- .sites.summary(object)
		s3 <- .spatial.summary(object)
		s4 <- .partition.summary(object)
		s5 <- .fidelity.summary(object)        	
		cat("object of class  :", class(object))		
		switch(choice,
			"all" = {
			cat(s1, s2, s3, s4, "\n")			
		}, "species" = {
			cat(s1, s3, s4, "\n")
		}, "sites" = {
			cat(s2, s3, s4, "\n")
		}, "partition" = {
			cat(s4, "\n")
		}, "fidelity" = {
			cat(s5, "\n")
		})	
	}
)

#	class VegsoupOptimstride
setMethod("summary",
	signature(object = "VegsoupOptimstride"),
		function (object, oc.treshold = 2, silent = FALSE) {
			#	object <- opt; oc.treshold = 2
		obj <- object@optimstride
		args <- obj$settings$args
		met <- args$method
		ind <- obj$indicators
		ftt <- args$ft.treshold
		oct <- oc.treshold
		args$oc.treshold <- oct

		oc1 <- t(sapply(ind, function (x) sapply(x, function (x) sum(x))))
		oc2 <- t(sapply(ind, function (x) sapply(x, function (x) length(which(x >= oct)))))
		
		#	not a good guess!
		best.oc1 <- sapply(obj$ind, function (x) (sapply(x, max)[which.max(sapply(x, max))] ) )

		res <- list(optimclass1 = oc1, optimclass2 = oc2, best.optimclass1 = best.oc1, args = args)
	
		if (!silent) {
			cat("OptimStride results for k:", args$k)
			cat("\n\nOptimClass 1 (fisher test treshold: ", ftt, "):\n", sep = "")
			print(res$optimclass1)
	
			cat("\nOptimClass 2 (occurence treshold: ",
				oct, "):\n", sep = "")
			print(res$optimclass2)
			
			cat("\nBest OptimClass\n", sep = "")
			print(res$best.optimclass1)
		}
		return(invisible(res))
		}
)

if (!isGeneric("head")) {
setGeneric("head", function (x, ...)
	standardGeneric("head"))
}

setMethod("head",
    signature(x = "Vegsoup"),
    function (x, n = 6L, choice, typeof, ...) {
	    if (missing(choice))
	    	choice = "species"
    	CHOICES <- c("species", "sites")
    	choice <- CHOICES[pmatch(choice, CHOICES)]
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
if (!isGeneric("tail")) {
setGeneric("tail", function (x, ...)
	standardGeneric("tail"))
}

setMethod("tail",
    signature(x = "Vegsoup"),
    function (x, n = 6L, choice, typeof, ...) {
	    if (missing(choice))
	    	choice = "species"
    	CHOICES <- c("species", "sites")
    	choice <- CHOICES[pmatch(choice, CHOICES)]
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