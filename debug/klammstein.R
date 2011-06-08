#	init vegbase
source("/Users/roli/dropbox/Rpackages/vegsoup/debug/init.R")

#	load data	
setwd("~/dropbox/Rpackages/vegsoup/debug")

load("species.Rdata")
load("taxonomy.Rdata")
load("sites.Rdata")
qry <- Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet"))
dta <- VegsoupData(qry)

dta <- Layers(dta, collapse = c("hl", "wl", "hl", "wl"), verbose = TRUE)

#	ind <- Indspc(dta)
#	ind <- Indpower(dta)
#	table(getSitesRaw(dta)$substrate)
#	sub <- dta[dta@sites.raw$substrate == "xyl",]
#std <- Stride(dta,
#	partiton.methd = "flexible",
#	fidelity.method = "r.g", stride = 30)
#.plotVegsoupSpeciesIndicators(std)

std <- Stride(dta, verbose = TRUE, stride = 5)
.plotVegsoupSpeciesIndicators(std)

k = 6

prt <- VegsoupDataPartition(dta, k = k, method = "optpart")
#prt <- Optindval(prt)
#	prt.isopam <- VegsoupDataPartition(dta, k = k, method = "isopam")


fid.prt <- Fidelity(prt)
tex.fid.prt.species <- Latex(fid.prt, choice = "species")
tex.fid.prt.sites <- Latex(prt, choice = "sites")
tex.species.recursive <- Latex(prt, choice = "species", recursive = TRUE)

#	Confus(prt, prt.opt)
#	write.csv2(tab$tab, paste(prt@method, ".csv", sep = ""))
#	write.csv2(tab.opt$tab, paste(prt@method, "opt.csv", sep = ""))

#LaTex.input <- vector("numeric", length = getK(prt))

#basic <- c("cov", "plsy", "plsx", "expo", "slope",
#	"date", "location", "elevation", "relief",
#	"vegetation", "substrate", "stones", "tree.diameter",
#	"tree.twig", "bark", "pl.meta")
#basic.tex.names <- c("Deck.", "b", "l", "Exp.", "Ink.",
#	"Datum", "Reg.", "msm", "Rel.", "Veget.", "Subst.",
#	"Steine", "B. Durchm", "Z. Durchm.", "Borke", "Zusatz")
	
#for (i in 1:k) {
#	tex <- VegsoupDataLaTexPipe(prt, i, "part", type = "free",
#		basic = basic, basic.tex.names = basic.tex.names, pwidth = 10) 
#	LaTex.input[i] <- tex@file.name
#}

#LaTex.input <- sapply(LaTex.input,
#	function (x) paste(paste("\\input{", x,"}",
#		sep = ""), "\n\t\\clearpage"))

#LaTex.input <- gsub("./Tex/", "", LaTex.input)
#con <- file("./Tex/LaTex_input.tex", "w")
#	writeLines(LaTex.input, con)
#close(con)	

