#	from list to Coverscale
setAs("list", "Coverscale", def = function (from) {
	#	ordinal
	if (!is.null(from[[2]]) & !is.null(from[[3]])) {
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = as.character(from[[2]]),
			lims = as.numeric(from[[3]])						
			)
	}		
	#	continous
	if (is.null(from[[2]]) & is.null(from[[3]])) { # 			
		res <- new("Coverscale",
			name = as.character(from[[1]]),
			codes = NULL,
			lims = NULL						
			)			
	}
	return(res)
})

#	from coenoflex to Vegsoup
as.Vegsoup.coenoflex <- function (obj) {

require(coenoflex)

spc <- obj$veg
spc[spc > 0 & spc <= 0.2] <- 0.2 #! document this
spc <- round(spc, 1) # ... and that
sts <- obj$site

n.plt <- nrow(spc)
n.spc = ncol(spc)

abbr <- gsub(" ", "0", paste0("spc", format(1:n.spc, width = 2)))
taxon <- gsub(" ", "0", paste0("species", format(1:n.spc, width = 2)))
plot <- gsub(" ", "0", paste0("plt", format(1:n.plt, width = 2)))

dimnames(spc) <- list(plot, abbr)
mode(spc) <- "character"

spc <- data.frame(abbr = colnames(spc), layer = "0l", comment = "", t(spc))
spc <- stack.species(spc, absence = "0")
sts <- stack.sites(data.frame(plot = plot, sts))

txa <- taxonomy(data.frame(abbr, taxon))

res <- Vegsoup(spc, sts, txa, "percentage")

return(res)

}

setAs(from = "coenoflex", to = "Vegsoup",
	def = function (from) {
		as.Vegsoup.coenoflex(from)
	}
)

