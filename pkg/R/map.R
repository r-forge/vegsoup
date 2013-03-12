".map" <- function (obj, database, ...) {
	require(maps)
	require(mapdata)
	if (missing(database)) database = "world"
	if (database != "world") require(mapdata)
	
	DATABASES <- c("world", "worldHires", "world2Hires")
	database <- match.arg(database, DATABASES)	
	#	gmap(SpatialPointsVegsoup(obj))
	res <- maps::map(database = database,
		xlim = bbox(obj)[1, ], ylim = bbox(obj)[2, ],
		...)
	points(SpatialPointsVegsoup(obj))
	return(invisible(res))
}

#if (!isGeneric("map")) {
setGeneric("map",
	function (obj, ...)
		standardGeneric("map")
)
#}

setMethod("map",
	signature(obj = "Vegsoup"),
	function (obj, ...) {
		.map(obj, ...)
	}
)