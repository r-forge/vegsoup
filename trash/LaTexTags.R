#	function adding summary statistics

LaTex.tags <- function (x, tag.species, tag.treshold)
{
#	to be reimplemented
#	drop prefix
#	order layers
#	build syntaxa caption
#	x = object of class tex.species
#	tag.species = TRUE

if (!inherits(x, "LaTex.species"))
	stop("Need object of class LaTex.species.resort")

attributes(x@taxa) <-
	list(tag = LaTex.species.tag(x, tag.species, tag.treshold))

return(x)
}

#	returns LaTex formatting for species as character vector

LaTex.species.tag <- function (x, tag.species, tag.treshold)
{
#	debug	
#	x = tex.species
#	tag.treshold = 0.5
	y <- commons(x, tag.treshold)
#	create data.frame with columns freq and occu
	com <- data.frame(abbr = x@abbr, freq = x@freq,
		stringsAsFactors = FALSE)
	
	com <- cbind(com, occu =
		y$occu[match(com$abbr, y$occu$abbr),2])
	com$tag <- com$occu > tag.treshold

#if (dim(com)[1] "")

if (tag.species)
{
	#	\\\\ line break
	tag <- paste("\\\\ \\scriptsize{\\texttt{(",
		paste(
			"\\bfseries{", # not working!
			round(com$freq * 100 / com$occu, 1),
			"}: ",
			paste(com$freq, "/", com$occu, sep = ""),
			sep = ""),
			")",
			" ", x@tabdev, x@spread,
		"}}", sep = "")
	} else {
	tag <- ""
}

#	retrieve taxanames from class of getClass("LaTex.species")
res <- x@taxa

#	tag all taxa first and index to this vector
bold.taxa <- paste("\\textit{\\textbf{", res,	"}}", sep = "")

if (tag.species)
{
	res[com$tag] <- paste(bold.taxa[com$tag], tag[com$tag])
} else {
	res  <- bold.taxa
}
#	add dots to getClass(class(x))
res[x@dots] <- paste(res[x@dots], "\\dotfill")

return(res)
}