#	translates database format to basic class structure
#	species and sites records are stored in long format
#	validity methods are implemented

setClass("Vegsoup",
	representation(
	species = "data.frame", # in long format, casting by method
	sites = "data.frame", # melt method
	taxonomy = "data.frame", # in long format
	coverscale = "Coverscale", #renamed
	layers = "character",
	group = "integer",
#	sp.raster = "raster",
	sp.points = "SpatialPointsDataFrame",
	sp.polygons = "SpatialPolygonsDataFrame"),
	
	validity = function (object) {
		if (length(object@taxonomy) > 0) {
			valid.abbr <-
				all(object@species$abbr %in% object@taxonomy$abbr)
			valid.sites <-
				all(sort(unique(object@species$plot)) == sort(rownames(object@sites)))
			if (valid.abbr && valid.sites) {
#				cat("\ntaxomomy lookup table matching",
#					"\nspecies and sites matching",
#					"\nslots: species, sites and taxonomy",
#					"assigned")
				TRUE
			}
			else {
				if (!valid.abbr) {
					warning("taxomomy lookup table has",
						"non matching taxa!")
					FALSE		
				}
				if (!valid.sites) {
					warning("\nspecies and sites has",
						"non matching plots!")
					FALSE
				}
			}
	}
	else {
			warning("no taxonomy lookup table supplied!\n")
		 }
	}
)