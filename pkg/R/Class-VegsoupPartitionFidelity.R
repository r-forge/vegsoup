setClass("VegsoupPartitionFidelity",
	representation(
	stat = "matrix",
	fisher.test = "matrix",
	lowerCI = "matrix",
	upperCI = "matrix",
	nboot = "integer",
	method = "character"),
	contains = "VegsoupPartition"
	)

#	showClass("VegsoupPartitionFidelity")