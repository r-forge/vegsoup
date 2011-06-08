#	init vegsoup
source("/Users/roli/dropbox/Rpackages/vegsoup/debug/init debug.R")

#	load data	
setwd("~/dropbox/Rpackages/vegsoup/debug")

load("species.Rdata")
load("taxonomy.Rdata")
load("sites.Rdata")
qry <- Vegsoup(species, sites, taxonomy,
	scale = list(scale = "frequency"))
dta <- VegsoupData(qry)

df <- cbind(y = log(rowSums(as.binary(dta))), x = Sites(dta)$altitude)
df <- df[df[,2] != 0,]
plot(y ~x, data = df)
fit <- loess(y ~x, data = as.data.frame(df))$fitted
lines(spline(df[,2], fit))

dta <- dta[getDistconnected(dta) == 1,]
dta <- dta[rowSums(dta) > 3, ]
dta <- dta[ ,colSums(dta) > 3]

#	ind <- Indspc(dta)
#	ind <- Indpower(dta)
#	table(getSitesRaw(dta)$substrate)
#	sub <- dta[dta@sites.raw$substrate == "xyl",]
#std <- Stride(dta,
#	partiton.methd = "flexible",
#	fidelity.method = "r.g", stride = 30)
#.plotVegsoupSpeciesIndicators(std)

k = 26

prt <- VegsoupDataPartition(dta, k = k, method = "flexible")
#	prt.isopam <- VegsoupDataPartition(dta, k = k, method = "isopam")
#	prt.opt <- Optindval(prt)

fid.prt <- Fidelity(prt)
summary(fid.prt)
tex.fid.prt <- Latex(fid.prt)
tex <- Latex(prt, choice = "sites")
sig.opt <- SigFidelity(prt)

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

