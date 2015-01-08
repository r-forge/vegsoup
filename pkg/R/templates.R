#	laTex template to warp around output of Latex method
template <- function() {
	file <- system.file("extdata", "template.tex", package = "vegsoup")
	con <- file(file)
		r <- readLines(con)
	close(con)
	return(r)
}
