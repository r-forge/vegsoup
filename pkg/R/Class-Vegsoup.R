#	translates database format to basic class structure
#	species and sites records are stored in long format
#	validity methods are implemented

setClass("Vegsoup",
	representation(
	species.long = "data.frame", # in long format, casting by Vegsoup
	sites.long = "data.frame", # in long format, casting by Vegsoup
	taxonomy = "data.frame", # in long format
	scale = "list",
	layers = "character",
	group = "integer",
#	sp.raster = "raster",
	sp.points = "SpatialPointsDataFrame",
	sp.polygons = "SpatialPolygonsDataFrame"),
	
	validity = function (object) {
		if (length(object@taxonomy) > 0) {
			valid.abbr <-
				all(object@species.long$abbr %in% object@taxonomy$abbr)
			valid.sites <-
				all(object@species.long$plot %in% object@sites.long$plot)

			if (valid.abbr && valid.sites) {
#				cat("\ntaxomomy lookup table matching",
#					"\nspecies and sites matching",
#					"\nslots: species, sites and taxonomy",
#					"assigned")
				TRUE
			} else {
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
		} else {
			warning("no taxonomy lookup table supplied!\n")
		}
	}
)