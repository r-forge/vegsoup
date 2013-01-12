#	private class not exposed to the user
#	used to allow slot 'decostand' to contain NULL 
setClassUnion("decostand.method", c("character", "NULL"))

setClass("decostand", representation(method = "decostand.method"))

#	class definition
setClass("VegsoupData",
	representation(
	decostand = "decostand",
	dist = "character"),
	contains = "Vegsoup")#,
#	validity = function (object)#
#	{
#	})