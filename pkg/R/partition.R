#	running partition vector
if(!isGeneric("Partitioning")) {
setGeneric("Partitioning",
	function (obj)
		standardGeneric("Partitioning")
)
}

#	retrieve or set slot part
setMethod("Partitioning",
	signature(obj = "VegsoupPartition"),
	function (obj) obj@part
)

#	replace slot part
if(!isGeneric("Partitioning<-")) {
setGeneric("Partitioning<-",
	function (obj, value, ...)
		standardGeneric("Partitioning<-")
)
}
setReplaceMethod("Partitioning",
	signature(obj = "VegsoupPartition", value = "numeric"),
	function (obj, value) {
		if (length(value) != length(Partitioning(obj))) {
			stop("\n replacement does not match in length")
		}
		if (is.null(names(value))) {		
			names(value) <- rownames(obj)
		} else {
			if (length(intersect(names(value), rownames(obj))) != nrow(obj)) {
				stop("\n if value has names, these have to match rownames(obj)")
			} else {
				value <- value[match(rownames(obj), names(value))]
			}
		}
		obj@part <- value
		obj@k <- length(unique(value))
		
		return(obj)		
	}
)

#	extended getter method
#if(!isGeneric("Partition")) {
setGeneric("Partition",
	function (obj, value, ...)
		standardGeneric("Partition")
)
#}

setMethod("Partition",
	signature(obj = "VegsoupPartition"),
	function (obj, value) {	
		obj[obj@part == value, ]
	}		
)
