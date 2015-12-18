setClass("VegsoupPartition",
	representation(
	#	add specific slot for classes returned by method
	part = "numeric", # rename to partition
	partitioning.method = "character",
	k = "numeric"),   # drop slot and replace with getter method!
	validity = function (object) { all(names(partitioning(object)) == rownames(object)) },
	contains = c("Vegsoup")
)

