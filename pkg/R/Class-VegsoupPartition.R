#	drop slot binary?
#	instead use decostand(obj) = "pa"

#	class definition
setClass("VegsoupPartition",
	representation(
	#	add specific slot for classes returned by method
	part = "numeric",	# rename to partition
	method = "character",
	k = "numeric"), # drop slot and replace with getter method!
	#	we demand names
	#	validity = function (object) { !is.null(names(Partitioning(object))) },
	#	and order
	validity = function (object) { all(names(Partitioning(object)) == rownames(object)) },
	contains = c("Vegsoup"))
	
	


