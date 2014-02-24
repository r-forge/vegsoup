#	private class not exposed to the user
#	used to allow slot 'decostand' to contain NULL 
setClassUnion("decostand.method", c("character", "NULL"))

setClass("decostand", representation(method = "decostand.method"))

#	class definition
setClass("Vegsoup",
	representation(
	species = "Species", # changed
	sites = "data.frame", # melt method
	taxonomy = "data.frame", # change to class "Taxonomy"
	coverscale = "Coverscale", #renamed
	decostand = "decostand",
	dist = "character",
	layers = "character",
	group = "integer",
	sp.points = "SpatialPointsDataFrame",
	sp.polygons = "SpatialPolygonsDataFrame")
)