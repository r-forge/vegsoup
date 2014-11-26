#if (!isGeneric("rbind")) {
setGeneric("species",
	function (obj, ...)
		standardGeneric("species")
)
#}

#if (!isGeneric("species<-")) {
setGeneric("species<-",
	function (obj, value)
		standardGeneric("species<-")
)
#}

#if (!isGeneric("abbr")) {
#	get or set taxon abbreviation
setGeneric("abbr",
	function (obj) {
		standardGeneric("abbr")
	}	
)
#}

#	we set the generic here,
#   classes "Sites", "Taxonomy" and "SpeciesTaxonomy"
#	then define methods for their classe
if (!isGeneric("rbind")) {
setGeneric("rbind",
		function (..., deparse.level = 1)
		standardGeneric("rbind"),
		signature = "...")
}

setMethod("species",
    signature(obj = "Species"),
    function (obj) {
    	obj@data
    }	
)

setMethod("species",
    signature(obj = "data.frame"),
    function (obj) {
    	new("Species", data = obj)
    }
    
)

setMethod("species",
    signature(obj = "matrix"),
    function (obj) {
    	new("Species",
    	data = as.data.frame(obj, stringsAsFactors = FALSE))
    }
    
)

setMethod("species",
    signature(obj = "character"),
    function (obj, ...) {
    	new("Species",
    	data = read.csv(obj, ...))
    }
    
)

setMethod("species",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) species(slot(obj, "species")) # ? slot(obj, "species")
)

setReplaceMethod("species",
	signature(obj = "SpeciesTaxonomy", value = "data.frame"),
	function (obj, value) {
		value <- species(value)
		sel <- match(unique(value$abbr), taxonomy(obj)$abbr)
		new("SpeciesTaxonomy",
			species = value,
			taxonomy = taxonomy(taxonomy(obj)[sel, ]))
	}
)

setReplaceMethod("species",
	signature(obj = "SpeciesTaxonomy", value = "Species"),
	function (obj, value) {
		sel <- match(unique(value$abbr), taxonomy(obj)$abbr)
		new("SpeciesTaxonomy",
		species = value,
		taxonomy = taxonomy(taxonomy(obj)[sel, ]))
	}
)

setMethod("species",
    signature(obj = "Vegsoup"),
    function (obj) {
    	slot(obj, "species")
    }	
)

setReplaceMethod("species",
	signature(obj = "Vegsoup", value = "SpeciesTaxonomy"),
	function (obj, value) {
		warning("not implemented yet")
		return(obj)		
	}
)

#	only if value intersects Taxonomy(obj)
setReplaceMethod("species",
	signature(obj = "Vegsoup", value = "Species"),
	function (obj, value) {
		#! if taxonomy(obj) is renamed
		# test <- any(is.element(abbr(taxonomy(obj)), unique(abbr(value)))) 
		test1 <- is.element(unique(abbr(value)), Taxonomy(obj)$abbr)
		test2 <- is.element(unique(value$plot), rownames(obj))
		stopifnot(any(test1) & any(test1))
		
		#	if we need a subset of plots
		obj <- obj[match(unique(value$plot), rownames(obj)), ]
		
		obj@species <- value
		value <- Taxonomy(obj)[Taxonomy(obj)$abbr %in% unique(abbr(obj)), ]
		obj@taxonomy <- value #! if slots becomes class "Taxonomy" taxonomy(value)		 
		return(obj)		
	}
)

setMethod("show",
    signature(object = "Species"),
    function (object) {
		cat("object of class   :",
			class(object))
		cat("\nnumber of species :",
			length(unique(species(object)$abbr)))
		cat("\nnumber of sites   :",
			length(unique(species(object)$plot)))		
		cat("\nshow only first",
			ifelse(nrow(object@data) <= 6, nrow(object@data), 6),
			"rows\n\n")
		print(head(object@data, n = 6L))
    }
)

setMethod("[",
    signature(x = "Species",
    i = "ANY", j = "ANY", drop = "missing"),
    function (x, i, j, ..., drop = FALSE) {
    	species(x@data[i, j, ...])
    }
)

setMethod("$",
    signature(x = "Species"),
	function(x, name) {
		if (!("data" %in% slotNames(x))) {
			stop("no $ method for object without slot data")
		}
		return(x@data[[name]])
	}
)

setReplaceMethod("$",
	signature(x = "Species"),
	function (x, name, value) {
 		x@data[[name]] <- value 	
		return(x)		
	}
)

setMethod("abbr",
    signature(obj = "Species"),
    function (obj) {
    	obj$abbr
    }	
)

".rbind.Species" <- function (..., deparse.level = 1) {
	allargs <- list(...)	
	x <- do.call("rbind", lapply(allargs, species))
	if (any(duplicated(x))) {
		message("duplicates found: ")
		print(x[duplicated(x), ])
	}
	x <- x[order(x$plot, x$layer, x$abbr), ]	
	return(species(x))

}


setMethod("rbind",
    signature(... = "Species"),
	.rbind.Species
)

#	VegsoupVerbatim methods
setOldClass("VegsoupVerbatim")

setMethod("species",
    signature(obj = "VegsoupVerbatim"),
    function (obj) {
		r <- data.frame(abbr = rownames(x),
				layer = NA,
				taxon = NA, x,
				check.names = FALSE, stringsAsFactors = FALSE)
						
		if (length(grep("@", rownames(x))) > 0 ) {
			a <- strsplit(as.character(r$abbr), "@")			
			r$abbr <- sapply(a, "[[", 1)		
			r$layer <- sapply(a, "[[", 2)			
		}
		r <- stackSpecies(r)[, 1:4]
		return(r)
	}
)

#if (!isGeneric("SpeciesList")) {
setGeneric("SpeciesList",
	function (obj, layered)
		standardGeneric("SpeciesList")
)
#}
setMethod("SpeciesList",
    signature(obj = "Vegsoup"),
    function (obj, layered = FALSE) {
    	if (missing(layered)) {
    		layered <- FALSE
    	}
    	if (layered) {
	    	res <- species(species(obj)) #! get slot data
    		res <- unique(res[c("abbr", "layer")])
    		res$taxon <- Taxonomy(obj)[res$abbr, ]$taxon
	    	res <- res[, c("abbr", "taxon", "layer")]
	    	res <- res[order(res$layer, res$taxon),]	    				
    	} else {
    		res <- Taxonomy(obj)[]	
    	}
    	rownames(res) <- seq_len(nrow(res))
    	return(invisible(res))	
	}
)

#if (!isGeneric("SpeciesList")) {
setGeneric("relevee",
	function (obj, plot, format = FALSE)
		standardGeneric("relevee")
)
#}
setMethod("relevee",
    signature(obj = "Vegsoup"),
    function (obj, plot, format) {
    	if (missing(plot)) {
    		i <- 1
    		message("return first plot in data set: ", rownames(obj)[i])    		
    	} else {
    		if (is.numeric(plot)) {
    			stopifnot(plot %in% 1:nrow(obj))
    			i <- plot
    		}
    		if (inherits(plot, "character")) {
    			stopifnot(plot %in% rownames(obj))
    			i <- which(rownames(obj) == plot)
    		}
    	}
    	x <- obj[i, ]


		#	header
	    h <- cbind(coordinates(x), Sites(x))
	    
	    h <- data.frame(variable = names(h), value = t(h)[, 1])
	    rownames(h) <- seq_len(nrow(h))
		
		#	species
		l <- species(species(x))
    	l$taxon <- Taxonomy(obj)[l$abbr, ]$taxon
	    l <- l[order(l$layer, l$taxon), ]	    				
    	l <- l[, c("taxon", "layer", "cov")]
    	rownames(l) <- seq_len(nrow(l))
    	l$layer[duplicated(l$layer)] <- ""
    	
    	r <- list(sites = h, species = l)    		
    	
    	if (format) {
    		r1 <- as.matrix(r$species)
			r2 <- as.matrix(r$sites)
			
			w <- apply(apply(r1, 2, nchar), 2, max)
			w1 <- sapply(colnames(r1), nchar)
			w1 > w
			w[w1 > w] <- w1[w1 > w]
			
			r1 <- rbind(colnames(r1), r1)
			dimnames(r1) <- NULL
			r1[, 1] <- str_pad(r1[, 1], w[1] + 1, side = "right", pad = " ")
			r1[, 2] <- str_pad(r1[, 2], w[2] + 1, side = "right")
			r1[, 3] <- str_pad(r1[, 3], w[3], side = "right")
			
			r1 <- apply(r1, 1, function (x) paste(x, collapse = ""))
			r2 <- paste(apply(r2, 1, function (x) paste (x, collapse = ": ")), collapse = ", ")
			
			r <- c(r1, " ", r2)	
    	}    	

    	return(r)
    		
	}
)

