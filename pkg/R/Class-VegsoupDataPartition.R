#	class definition
setClass("VegsoupDataPartition",
	representation(
	#	add specific slot for classes returned by method
	part = "numeric",	# rename to partition
	method = "character",
	k = "numeric", # drop slot and replace with getter method!
	binary = "logical"),
	contains = c("VegsoupData"))
