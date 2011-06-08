#	class definition
setClass("VegsoupData",
	representation(
	species = "data.frame",	# matrix, rename to species
#	species.vdm = "data.frame",	# replace with getter method
#	species.pa = "data.frame",	# replace with getter method
	sites = "data.frame"),	# rename to sites
	contains = "Vegsoup")#,
#	validity = function (object)#
#	{
#	any(is.na(rownames(object@sites.raw) %in%
#		rownames(object@species.raw)))
#	cat("Species and plot order incorrect")		
#	})