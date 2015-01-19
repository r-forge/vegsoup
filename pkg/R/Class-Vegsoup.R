#	private class not exposed to the user
#	used to allow slot 'decostand' to contain NULL 
setClassUnion("decostand.method", c("character", "NULL"))

setClass("decostand", representation(method = "decostand.method"))

#	class definition
setClass("Vegsoup",
	representation(
	species = "Species",
	sites = "data.frame",
	taxonomy = "Taxonomy",
	coverscale = "Coverscale",
	decostand = "decostand",
	dist = "character",
	layers = "character",
	group = "integer",
	sp.points = "SpatialPointsDataFrame",
	sp.polygons = "SpatialPolygonsDataFrame")
)