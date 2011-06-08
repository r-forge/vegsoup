setClass("VegsoupDataLaTexSpecies",
	representation(
	species = "matrix", # rename to 'species.latex'
	caption = "character",
	ruler = "numeric",
	dots = "numeric",
	freq = "numeric",
	tabdev = "character",
	spread = "character",
	taxa = "character", 
	abbr = "character",
	layer = "character"))