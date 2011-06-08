setClass("VegsoupDataLaTex",
	representation(
	species = "VegsoupDataLaTexSpecies",
	sites = "VegsoupDataLaTexSites",
	file.name = "character",
	longtable = "logical",
	part = "numeric",
	LaTex = "list") # to be assigend by '.VegsoupLaTexParse'
	)
