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

#	vegan defines:
#	coverscale(x, scale=c("Braun.Blanquet", "Domin", "Hult", "Hill",
#	"fix","log"), maxabund)
#if (!isGeneric("coverscale")) {
setGeneric("coverscale",
	function (x, scale = c("Braun.Blanquet", "Domin", "Hult", "Hill",
	"fix","log"), maxabund)
	standardGeneric("coverscale"))
#}
#if (!isGeneric("coverscale <-")) {
setGeneric("coverscale<-",
	function (x, value)
		standardGeneric("coverscale<-")
)
#}
#if (!isGeneric("coverscale")) {
setGeneric("is.ordinal", function (x)
	standardGeneric("is.ordinal"))
#}
#if (!isGeneric("coverscale")) {
setGeneric("is.continuous", function (x)
	standardGeneric("is.continuous"))
#}
setMethod("is.ordinal",
    signature(x = "Coverscale"),
    function (x) {
  		!is.null(x@codes) & !is.null(x@codes)
    }
)
setMethod("is.continuous",
    signature(x = "Coverscale"),
    function (x) {
  		is.null(x@codes) & is.null(x@codes)
    }
)