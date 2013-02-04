#	private class not exposed to the user
#	used to allow slot 'decostand' to contain NULL 
setClassUnion("decostand.method", c("character", "NULL"))

setClass("decostand", representation(method = "decostand.method"))

#	class definition
setClass("VegsoupData",
	representation(
	species = "data.frame", # in long format, casting by method
	sites = "data.frame", # melt method
	taxonomy = "data.frame", # in long format
	coverscale = "Coverscale", #renamed
	decostand = "decostand",
	dist = "character",
	layers = "character",
	group = "integer",
#	sp.raster = "raster",
	sp.points = "SpatialPointsDataFrame",
	sp.polygons = "SpatialPolygonsDataFrame")
)