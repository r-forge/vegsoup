basic <- c("cov", "hcov", "mcov", "hhl1", "hhl2","expo", "slope", "plsx", "plsy", "date")

#	create Latex tables
parts <- partition@k
LaTex.input <- c()

plain <- vector("list", length = length(1:parts))

setwd("~/Documents/Rpackages/vegbase/debug/Tex")

for (i in 1:parts) {
	#	i = 1
	file.name <- paste("./LaTex/part", i, sep = "")
	LaTex.input <- c(LaTex.input, file.name)
	plain[[i]] <- vegbase.LaTex(x, y, i,
		file.name = file.name,
		order.layer = order.layer,
		abundant.2.top = abundant.2.top,
		add.rule = FALSE,
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
		basic.tex.names = basic.texnames,
		type = type,
		txpwidth = txpwidth)
}

LaTex.input <- sapply(LaTex.input,
	function (x) paste(paste("\\input{", x,"}",
		sep = ""), "\n\t\\clearpage"))

#hm.latex <- vegbaseHeatmap2LaTex(hm, spc, type)
#latex(hm.latex, file = "./vegbase/syn.tex",
#	longtable = TRUE, booktabs = TRUE,
#	lines.page=20)

#LaTex.input <- c("\\input{vegbase/syn.tex}",
#	"\\clearpage", LaTex.input)

con <- file("./LaTex/input.tex", "w")
	writeLines(LaTex.input, con)
close(con)	

#	csv check out files
names(plain) <- paste("part", 1:ifelse(length(plain) > 0, length(plain), 1))

for (i in 1:length(plain))
{
write.csv2(plain[[i]]$species,
	file = paste("./plain/", names(plain[i]),
		" species.csv", sep = ""))
write.csv2(plain[[i]]$sites,
	file = paste("./plain/", names(plain[i]),
	" sites.csv", sep = ""))
}

setwd("~/Documents/Rpackages/vegbase/debug")