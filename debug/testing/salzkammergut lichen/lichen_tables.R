#	vegbase sites setup
basic <- c("location", "vegetation", "elevation", "substrate", "cov", "tree.twig", "relief")
#basic <- attributes(fit$terms)$term.labels
#basic <- c("cov", "expo", "slope",
#	"elevation", "relief", "vegetation", "substrate",
#	"stones", "tree.diameter", "tree.twig",	"bark")
	
#basic.tex.names <- c("cov", "expo", "slope",
#	"elevation", "relief", "vegetation", "substrate",
#	"stones", "tree diameter", "tree twig",	"bark")

#	create Latex tables
parts <- partition@k
LaTex.input <- c()

checkout <- vector("list", length = length(1:parts))

setwd("~/Documents/Rpackages/vegbase/debug/Tex")

for (i in 1:parts) {
	#	i = 5
	file.name <- paste("./vegbase/part", i, sep = "")
	LaTex.input <- c(LaTex.input, file.name)
	checkout[[i]] <- vegbase.LaTex(partition, sites, i,
		file.name = file.name,
		order.layer = order.layer,
		abundant.2.top = abundant.2.top,
		add.rule = TRUE,
		table.method = table.method,
		tag.species = tag.species,
		# a number specifing frequency species occurence threshold in full data set
		# range from 0 to inf, adds a frequency description
		# in tex file, zero adds tag to all species!
		tag.treshold = tag.treshold,
		# a proportion, prints dooted filled lines per table nor per full data
		# range from 0 to 1
		abundance.treshold = abundance.treshold,
		basic = basic,
		basic.tex.names = basic.tex.names,
		type = "free",
		txpwidth = 70)
}

LaTex.input <- sapply(LaTex.input,
	function (x) paste(paste("\\input{", x,"}",
		sep = ""), "\n\t\\clearpage"))

hm.latex <- vegbaseHeatmap2LaTex(hm, spc, "free")
latex(hm.latex, file = "./vegbase/syn.tex",
	longtablde = TRUE, booktabs = TRUE,
	lines.page=20)

LaTex.input <- c("\\input{vegbase/syn.tex}",
	"\\clearpage", LaTex.input)

con <- file("./vegbase/LaTex_input.tex", "w")
	writeLines(LaTex.input, con)
close(con)	

#	plain tables
names(checkout) <- paste("part", 1:ifelse(length(checkout) > 0, length(checkout), 1))

for (i in 1:length(checkout))
{
write.csv2(checkout[[i]]$species,
	file = paste("./checkout/", names(checkout[i]),
		"species.csv", sep = ""))
write.csv2(checkout[[i]]$sites,
	file = paste("./checkout/", names(checkout[i]),
	"sites.csv", sep = ""))
}

setwd("~/Documents/Rpackages/vegbase/debug")