#if (!isGeneric("species")) {
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

setReplaceMethod("species",
	signature(obj = "Species", value = "data.frame"),
	function (obj, value) {
		value <- value[, c("abbr", "taxon")]
		
		r <- species(obj)
		a <- factor(r$abbr)
		i <- match(levels(a), value$taxon)
		nas <- is.na(i)

		if (any(nas)) {
			stop("value does not match for:\n", levels(a)[nas],
				"\npmatch returns:\n", value$taxon[pmatch(levels(a)[nas], value$taxon)],
				call. = FALSE)	
		}
		
		levels(a) <- value$abbr[i]
		r$abbr <- as.character(a)
		r <- species(r)
		
		return(r)
	}
)

setMethod("species",
    signature(obj = "SpeciesTaxonomy"),
    function (obj) slot(obj, "species")
)

setReplaceMethod("species",
	signature(obj = "SpeciesTaxonomy", value = "data.frame"),
	function (obj, value) {
		value <- species(value)
		sel <- match(unique(value$abbr), taxonomy(obj)$abbr)
		new("SpeciesTaxonomy",
			species = value,
			taxonomy = taxonomy(obj)[sel, ])
	}
)

setReplaceMethod("species",
	signature(obj = "SpeciesTaxonomy", value = "Species"),
	function (obj, value) {
		sel <- match(unique(value$abbr), taxonomy(obj)$abbr)
		new("SpeciesTaxonomy",
		species = value,
		taxonomy = taxonomy(obj)[sel, ])
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

#	only if value intersects taxonomy(obj)
setReplaceMethod("species",
	signature(obj = "Vegsoup", value = "Species"),
	function (obj, value) {
		#! if taxonomy(obj) is renamed
		# test <- any(is.element(abbr(taxonomy(obj)), unique(abbr(value)))) 
		t1 <- is.element(unique(abbr(value)), taxonomy(obj)$abbr)
		t2 <- is.element(unique(value$plot), rownames(obj))
		stopifnot(any(t1) & any(t2))
		
		#	if we need a subset of plots
		obj <- obj[match(unique(value$plot), rownames(obj)), ]
		
		obj@species <- value
		
		#	if we loose layers
		obj@layers <- unique(value$layer)
						
		#	at least we need to subset taxonomy
		value <- taxonomy(obj)[taxonomy(obj)$abbr %in% unique(abbr(obj)), ]
		obj@taxonomy <- value		 
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

setMethod("bind",
    signature(... = "Species"),
	function (..., deparse.level = 1) {
		allargs <- list(...)
		x <- do.call("rbind", lapply(allargs, species))
		if (any(duplicated(x))) {
			message("duplicates found: ")
			print(x[duplicated(x), ])
		}
		#	explicit ordering!
		x <- x[order(x$plot, x$layer, x$abbr), ]
		return(species(x))
	}
)

#	VegsoupVerbatim methods
setOldClass("VegsoupVerbatim")

setMethod("species",
    signature(obj = "VegsoupVerbatim"),
    function (obj) {
		r <- data.frame(abbr = rownames(obj),
				layer = NA,
				taxon = NA, obj,
				check.names = FALSE, stringsAsFactors = FALSE)
						
		if (length(grep("@", rownames(obj))) > 0 ) {
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
    	if (missing(layered))
    		layered <- FALSE

    	if (layered) {
	    	res <- species(species(obj)) #! get slot data
    		res <- unique(res[c("abbr", "layer")])
    		# we can't use the [-method because we want layer replicates
    		# these can easily by obtained by indexing rownames with characters
    		res$taxon <- taxonomy(taxonomy(obj))[res$abbr, ]$taxon
	    	res <- res[, c("abbr", "taxon", "layer")]
	    	res <- res[order(res$layer, res$taxon),]	    				
    	}
		else {
    		res <- taxonomy(taxonomy(obj))
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
	    h <- cbind(coordinates(x), sites(x))
	    
	    h <- data.frame(variable = names(h), value = t(h)[, 1])
	    rownames(h) <- seq_len(nrow(h))
		
		#	species
		l <- species(species(x))
    	l$taxon <- taxonomy(taxonomy(obj))[l$abbr, ]$taxon # see SpeciesList
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
