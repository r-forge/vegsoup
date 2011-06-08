setClass("VegbaseDataLaTex",
	representation(
	species = "matrix",
	sites = "matrix",
	sites.extended = "matrix",
	caption = "character", # extend list
	ruler = "numeric",
	dots = "numeric",
	freq = "numeric",
	tabdev = "character",
	spread = "character",
	taxa = "character", 
	abbr = "character",
	layer = "character"),
	contains = "VegbaseData")

#	class returned after parsing dumped files
setClass("VegbaseDataLaTexParse",
	representation(
	file.name = "character",
	longtable = "logical",
	LaTex = "list",
	contains = "VegbaseDataLaTex"))
