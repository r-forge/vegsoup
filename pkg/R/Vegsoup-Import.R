#	reverse geocoding
#	readLines(url("http://maps.google.com/maps/geo?q=1600+Straßham+Wilhering+CA&output=csv&key=abcdefg"), n=1, warn=FALSE)

.make.names <- function (x)  {
    x <- make.names(x, unique = FALSE)
    x <- lapply(strsplit(x, "\\."),
    function(x) if (length(x) > 1)
        substring(x, 1, 4)
    else x)
    x <- unlist(lapply(x,
    	function (x) if (length(x) > 1) 
        paste(x[seq(1, length(x))], collapse = " ")
    else x))
    x <- gsub("ssp  ", "", x, fixed = TRUE)
    x <- gsub("var  ", "", x, fixed = TRUE)
    x <- gsub("  ", " ", x, fixed = TRUE)
    x <- abbreviate(x, 8)
   	x <- make.names(x, unique = TRUE)
    x
}