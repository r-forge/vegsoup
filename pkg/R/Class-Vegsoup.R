#	private class not exposed to the user
#	used to allow slot 'decostand' to contain NULL 
setClassUnion("decostand.method", c("character", "NULL"))

setClass("decostand", representation(method = "decostand.method"))

#	class definition
setClass("Vegsoup",
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

#	coercion method
#	coercion to class Vegsoup is automatic as defined by the contains= argument
#	to do: documenation
setAs("Vegsoup", "list",
	def = function (from) {
		list(
		species = as.matrix(from, typeof = "character", mode = "Q"),
		sites = from@sites)
	}
)