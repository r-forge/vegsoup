read.XML <- function (file) {
	#	we've explict imports of used functions from package XML
	doc <- xmlRoot(xmlTreeParse(file))
	
	#	there are threee nodes
	#	names(doc) # we don't carry about node 'Template'
	
	#	split root into 2 nodes
	doc1 <- doc[["Plots"]]         # with two parent nodes, plots are children thereof
	doc2 <- doc[["Lookup_tables"]] # additional data, nodes to be converted into tables
	
	#	two functions to access content of parent nodes
	#	process parent node doc1
	xmlPlots <- function (x) {
		#	get a single node
		#	x <- doc1[[1]]
		
		#	this node has two children header_data and species_data,
		#	we denote them x and y, respectively
		y <- x[["header_data"]]
		x <- x[["species_data"]]
		
		#	sites (header_data)
		y <- xmlApply(y, xmlAttrs)
		#	we must have standard record
		i1 <- names(y) == "standard_record"
		#	and usually other (non-standard) attributes
		i2 <- !i1
		
		y1 <- y[i1][[1]]
		y1 <- cbind(name = names(y1), value = y1)
		rownames(y1) <- make.unique(rep("standard_record", nrow(y1)))
		
		y2 <- t(data.frame(y[i2]))
		y2 <- y2[, colnames(y2) == "name" | colnames(y2) == "value"]
		
		y <- rbind(y1, y2)
		
		#	releve_nr is in first position, we will also use it later for species
		p <- y[1, 2]
		y <- sites(cbind(p, y))
		
		#	we try to catch the date format
		d <- as.Date(variable(y, "date"), "%Y%m%d")
		if (!any(is.na(d)))
			variable(y, "date") <- d
		
		#	species (species_data)
		x <- t(xmlSApply(x, function (x) xmlSApply(x, xmlAttrs)))
		
		#	we need drop = FALSE if relevee consits of only 1 species record
		x <- species(cbind(p, x[, c(1,3,2), drop = FALSE]))
		
		return(list(x, y))
	}
	
	#	process parent node doc2
	xmlLookup <- function (x) {
		#	we receive the the root as argument x
		
		#	this node has five children
		#	we denote them x1, x2, ... x5, respectively
		x1 <- x[["Coverscale_list"]] # important
		x2 <- x[["Country_list"]]    # 
		x3 <- x[["Author_list"]]     #
		x4 <- x[["Project_list"]]    #
		x5 <- x[["Species_list"]]    # the most important
		
		#	coverscale (Coverscale_list)
		if (length(x1) == 1) {
			x11 <- xmlApply(x1[[1]], xmlValue)[1:2]
			x12 <- xmlApply(x1[[1]], xmlAttrs)[3:length(x1[[1]])]
			x12 <- t(data.frame(x12))
			x1 <- Coverscale(name = x11[2],	codes = x12[, 1], lims = x12[, 2])
		} else {
			x11 <- xmlSApply(x1, function (x) xmlSApply(x, xmlValue)[1:2], simplify = FALSE)
			x12 <- xmlSApply(x1, function (x) xmlSApply(x, xmlAttrs)[-c(1:2)])
			
			x1 <- sapply(1:length(x11), function (i) {
				r <- t(data.frame(x12[[i]]))
				r <- Coverscale(name = as.character(x11[[i]][2]), codes = r[, 1], lims = r[, 2])			
			})
			#	assign turboveg coverscale codes as list names
			names(x1) <- sapply(1:length(x1), function (x) x11[[x]][1])			
		}
		
		#	country (Country_list)
		x2 <- xmlSApply(x2, xmlAttrs)
	
		#	author (Author_list)
		x3 <- xmlSApply(x3, xmlAttrs)
	
		#	project (Project_list)
		x4 <- xmlSApply(x4, xmlAttrs)
			 
		#	reference list (Species_list)
		x5 <- xmlApply(x5, xmlAttrs)
		x5 <- t(data.frame(x5))
		
		#	taxonomy,data-frame-method requires specific names
		#	instead of 'nr' and 'name'
		colnames(x5)[c(1,3)] <- c("abbr", "taxon") 
		x5 <- taxonomy(x5)
		
		return(list(x1, t(cbind(x2, x3, x4)), x5))
	}
	
	#	run node functions
	xy <- xmlApply(doc1, xmlPlots)
	zz <- xmlLookup(doc2)
	
	x <- do.call("rbind", lapply(xy, "[[", 1)) # species
	y <- do.call("rbind", lapply(xy, "[[", 2)) # sites
	z <- zz[[3]]                               # taxonomy
	s <- zz[[1]]                               # cover scale
	p <- zz[[2]]                               # project attributes
	
	#	we need to worry about coverscale, if the data set apllies more than one
	if (length(s) > 1) {
		message("data set applies 2 or more coverscales, return list of objects")
						
		#	list of objects based on coverscale
		ss <- variable(y, "coverscale") # coverscale is recorded along 'standard_record'
		
		r <- sapply(unique(ss), function (ii) {	
			i <- names(ss)[ss == ii]
			
			xi <- unlist(sapply(i, function (ii) which(x$plot == ii), simplify = FALSE))
			xi <- x[xi, ]

			yi <- unlist(sapply(i, function (ii) which(y$plot == ii), simplify = FALSE))
			yi <- y[yi, ]
			
			zi <- taxonomy(SpeciesTaxonomy(xi, z))
			
			si <- s[[which(names(s) == ii)]]
			
			ri <- Vegsoup(xi, yi, zi, coverscale = si)
			
			ri$observer <- p["author_record", 2]
			ri$country <- p["country_record", 2]
			ri$project <- p["project_record", 2]

			return(ri)
		} )
		#	give sound names
		names(r) <- sapply(sapply(r, coverscale), slot, "name")		
	}
	else {
		r <- Vegsoup(x, y, z, coverscale = s)
			
		r$observer <- p["author_record", 2]
		r$country <- p["country_record", 2]
		r$project <- p["project_record", 2]
	}
		
	return(r)
}
