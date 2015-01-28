setGeneric("as.table")

".as.table.VegsoupPartition" <- function (x, ...) {
	allargs <- list(...)
	if (length(allargs) != 1)
		stop("accept only one column name or vector")
	
	n <- allargs[[1]]  # use first element anyway			
	j <- names(x) == n[1] # column in sites
	
	if (!any(j)) {
		#	'external' vector
 		if (length(n) != nrow(x))
			stop("length of supplied vector must match nrow(x)")
		else
			r <- table(as.character(n), partitioning(x))
	}
	else {
		if (any(j))
			r <- table(as.character(sites(x)[, which(j)]), partitioning(x))
		else
			stop("column not found")
	}	
	
	return(r)
}

setMethod("as.table",
    signature = "VegsoupPartition",
	.as.table.VegsoupPartition
)
