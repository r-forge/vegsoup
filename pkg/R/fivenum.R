#	summary statistics
#	Tukey Five-Number Summary
setGeneric("Fivenum",
	function (obj, na.rm = TRUE, recode = FALSE)
		standardGeneric("Fivenum")
)

setMethod("Fivenum",
	signature(obj = "VegsoupPartition"),
	function (obj, na.rm = TRUE, recode = FALSE) {
		if (recode & !is.null(decostand(obj))) {
			message("disregard decostand method for calculations")
		 	decostand(obj) <- NULL
		} 
		tmp <- as.numeric(obj)
		#	if (!na.rm)
		tmp[tmp == 0] <- NA
		tmp <- aggregate(tmp,
			by = list(Partitioning(obj)),
			FUN = function (x) fivenum(x, na.rm = TRUE), simplify = FALSE)
		part <- tmp[, 1]
		tmp <- tmp[, -1]
		res <- array(0, dim = c(dim(tmp)[2], dim(tmp)[1], 5),
			dimnames = list(names(tmp), part,
			c("min", "lower", "median", "upper", "max")))
		for (i in 1:5) {
			for (j in 1:nrow(res)) {
				#	j = 1; i = 1
				res[j, , i] <- sapply(tmp[, j], function (x) x[i])
			}
		}
		if (recode) {
			for (i in 1:dim(res)[3]) {
				tmp <- res[, , i]
				mode(tmp) <- "numeric"
				vals <- as.character(cut(tmp,
					breaks = c(0, coverscale(obj)@lims),
					labels = coverscale(obj)@codes,
					))
				tmp.i <- tmp
				mode(tmp.i) <- "character"
				tmp.i[] <- vals
				res[, , i] <- tmp.i
			}			
		res[is.na(res)] <- "."
		}
		return(invisible(res))	
	}
)
