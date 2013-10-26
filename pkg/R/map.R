".map" <- function (x, database, ...) {
	require(maps)
	require(mapdata)
	if (missing(database)) database = "world"
	if (database != "world") require(mapdata)
	
	DATABASES <- c("world", "worldHires", "world2Hires")
	database <- match.arg(database, DATABASES)	
	#	gmap(SpatialPointsVegsoup(x))
	res <- maps::map(database = database,
		xlim = bbox(x)[1, ], ylim = bbox(x)[2, ],
		...)
	points(SpatialPointsVegsoup(x))
	return(invisible(res))
}

#if (!isGeneric("map")) {
setGeneric("map",
	function (x, ...)
		standardGeneric("map")
)
#}

setMethod("map",
	signature(x = "Vegsoup"),
	function (x, database, ...) {
		.map(x, ...)
	}
)