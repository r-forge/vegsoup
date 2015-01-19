read.XML <- function (file) {
	#	we've explict imports of used functions from package XML
	doc <- xmlRoot(xmlTreeParse(file))
	
	#	there are threee nodes
	#	names(doc) # we don't carry abourt node 'Template'
	
	#	split root into 2 nodes
	doc1 <- doc[["Plots"]]         # with two parent nodes, plots are children thereof
	doc2 <- doc[["Lookup_tables"]] # additional data, nodes to be converted into tables
	
	#	two functions to access content of  parent nodes
	#	process parent node doc1
	xmlPlots <- function (x) {
		#	get a single node
		#x <- doc1[[node]]
		
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
		
		#	species (species_data)
		x <- t(xmlSApply(x, function(x) xmlSApply(x, xmlAttrs)))
		x <- species(cbind(p, x[, c(1,3,2)]))
		
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
		x11 <- xmlApply(x1[[1]], xmlValue)[1:2]
		x12 <- xmlApply(x1[[1]], xmlAttrs)[3:length(x1[[1]])]
		x12 <- t(data.frame(x12))
		x1 <- Coverscale(name = x11[2],	codes = x12[, 1], lims = x12[, 2])
		
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
	
	r <- Vegsoup(x, y, z, coverscale = zz[[1]])
	r$observer <- zz[[2]]["author_record", 2]
	r$country <- zz[[2]]["country_record", 2]
	r$project <- zz[[2]]["project_record", 2]  # not really needed
	
	return(r)
}
