setwd("~/Documents/Rpackages/vegbase/debug")

load("species.Rdata")
load("taxonomy.Rdata")
load("sites.Rdata")

query <- vegbase.query(species, sites, taxonomy)
#	summary(query)

species <- vegbase.species(query, scale = "frequency")
#	summary(species)
#	ord <- plot(species)

sites <- vegbase.sites(query)
#	summary(sites)

#	take subset
substrates <- table(sites@raw$substrate)
substrates <- substrates[names(substrates) %in% names(substrates[1])]
substrates <- sapply(names(substrates),
	function(x) which(sites@raw$substrate == x))
substrates <- sort(unlist(substrates))

#	subset frequent substrates types
species <- species[substrates,]
sites <- sites[substrates,]

#	plot
plot(species)

species.duleg <- vegbase.duleg(species, 10,
	dist = "bray")
plot(species.duleg)

#partition <- vegbase.partition(species, k = 1,
#	method = "pam", binary = FALSE, dist = "bray")



#	plot(partition)

#	create Latex tables
parts <- partition@k
LaTex.input <- c()

checkout <- vector("list", length = length(1:parts))

setwd("~/Documents/Rpackages/vegbase/debug/Tex")

for (i in 1:parts) {
	#	i = 1
	file.name <- paste("./vegbase/part", i, sep = "")
	LaTex.input <- c(LaTex.input, file.name)
	checkout[[i]] <- vegbase.LaTex(partition, sites, i,
		file.name = file.name,
		order.layer = FALSE,
		abundant.2.top = TRUE,
		add.rule = TRUE,
		table.method = "dca",
		tag.species = TRUE,
		# a number specifing frequency species occurence threshold in full data set
		# range from 0 to inf, adds a frequency description
		# in tex file, zero adds tag to all species!
		tag.treshold = 1,
		# a proportion, prints dooted filled lines per table nor per full data
		# range from 0 to 1
		abundance.treshold = 0.75,
		basic = basic,
		basic.tex.names = basic.tex.names,
		type = "free")
}

LaTex.input <- sapply(LaTex.input,
	function (x) paste(paste("\\input{", x,"}",
		sep = ""), "\n\t\\clearpage"))


con <- file("./vegbase/LaTex_input.tex", "w")
	writeLines(LaTex.input, con)
close(con)	

#	csv check out files
names(checkout) <- paste("part", 1:ifelse(length(checkout) > 0, length(checkout), 1))

for (i in 1:length(checkout))
{
write.csv2(checkout[[i]]$species,
	file = paste("./checkout/", names(checkout[i]),
		" species.csv", sep = ""))
write.csv2(checkout[[i]]$sites,
	file = paste("./checkout/", names(checkout[i]),
	"sites.csv", sep = ""))
}