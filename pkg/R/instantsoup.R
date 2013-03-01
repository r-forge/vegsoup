#	private class not exposed to the user
#	used to allow slot 'decostand' to contain NULL 
#setClassUnion("InstantSoup", c("Vegsoup", "Species", "Sites", "Taxonomy"))

#showClass("InstantSoup")

#setClass("SpeciesTaxonomyVirtual",
#	representation = representation("VIRTUAL")	
#)

instantsoup <- function (folder, sep = ";", dec = ",", coverscale) {
	
	if (missing(coverscale)) {
		coverscale = "braun.blanquet"
	}
	#folder <- "~/Documents/vegsoup-data/hohewand dta"
	files <- list.files(folder)
	paths <- list.files(folder, full.names = TRUE)
	
	x.file <- grep("species", files)
	y.file <- grep("sites", files)
	z.file <- grep("taxonomy", files)

	z <- "~/Documents/vegsoup-standards/austrian standard list 2008/austrian standard list 2008.csv"	
	if (!c(length(x.file) == 1 & length(y.file) == 1)) {
		stop("need both files for species and sites", call. = FALSE)
	}

	if (length(grep("wide", files[x.file])) > 0) {
		x <- stack.species(file = paths[x.file])		
	} else {
		x <- species(paths[x.file],	sep = sep, dec = dec)[,1:4]
	}

	if (length(grep("wide", files[y.file])) > 0) {
		y <- stack.sites(file = paths[y.file])		
	} else {
		y <- sites(paths[y.file], sep = sep, dec = dec)
	}
    
    if (length(z.file) == 0) {
    	xz <- SpeciesTaxonomy(x = x,
    	file.y = z)
    } else {
    	z <- taxonomy(z,sep = sep, dec = dec)
		xz <- SpeciesTaxonomy(x, z)	
    }
	 
	res <- Vegsoup(xz, y, coverscale = coverscale)
	return(res)		
	
}