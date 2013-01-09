#	private class not exposed to the user
#	used manly to allow slot 'decostand' to contain NULL 
setClassUnion("decostand.method", c("character", "NULL"))

setClass("decostand", representation(method = "decostand.method"))

#	add slot decostand
#	and method for slot decostand
#	decostand(obj)


#	class definition
setClass("VegsoupData",
	representation(
	species = "data.frame",
	decostand = "decostand",
	dist = "character"),
	contains = "Vegsoup")#,
#	validity = function (object)#
#	{
#	})