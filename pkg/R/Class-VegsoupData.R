#	class definition
setClass("VegsoupData",
	representation(
	species = "data.frame"),	# matrix, rename to species
#	had	sites = "data.frame"),	# rename to sites
	contains = "Vegsoup")#,
#	validity = function (object)#
#	{
#	any(is.na(rownames(object@sites.raw) %in%
#		rownames(object@species.raw)))
#	cat("Species and plot order incorrect")		
#	})