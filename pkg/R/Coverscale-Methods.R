#	Vegsoup validity check
#	!any(is.na(factor(x$cov, levels = scale$codes, labels = scale$lims)))
#	x$cov <- as.numeric(x$cov)
#	if (any(is.na(x$cov))) {
#	str(x$cov)
#	stop("there seems to be digits mixed with characters?")

Coverscale <- function (name, codes, lims) {
	if (missing(name)) {
		cat("buitin coverscales are:\n",
		paste(names(.COVERSCALES), collapse = "\n"))
	} else {
	if (missing(codes) & missing(lims)) {
		res <- .COVERSCALES[[match.arg(name, names(.COVERSCALES))]]
	} else {
		if (missing(codes) | missing(lims))	{
			stop("need both codes and lims", call. = FALSE)
		} else {
			if (length(codes) != length(lims)) {
				stop("length of codes and lims are not the same", call. = FALSE)
			} else {
				res <- list(
					name = as.character(name),
					codes = as.character(codes),
					lims = as.numeric(lims)
					)	
			}
		}
	}
	res <- as(res, "Coverscale")
	return(res)
	}
}

#if (!isGeneric("coverscale")) {
setGeneric("coverscale", function(obj)
	standardGeneric("coverscale"))
#}
#if (!isGeneric("coverscale <-")) {
setGeneric("coverscale<-",
	function (obj, value)
		standardGeneric("coverscale<-")
)
#}
#if (!isGeneric("coverscale")) {
setGeneric("is.ordinal", function(obj)
	standardGeneric("is.ordinal"))
#}
#if (!isGeneric("coverscale")) {
setGeneric("is.continuous", function(obj)
	standardGeneric("is.continuous"))
#}
setMethod("is.ordinal",
    signature(obj = "Coverscale"),
    function (obj) {
  		!is.null(obj@codes) & !is.null(obj@codes)
    }
)
setMethod("is.continuous",
    signature(obj = "Coverscale"),
    function (obj) {
  		is.null(obj@codes) & is.null(obj@codes)
    }
)