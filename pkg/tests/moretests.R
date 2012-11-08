library(vegsoup)
scale <- list(
	scale = "Braun-Blanquet", 
	codes = c("r", "+", "1",
		"2m", "2a", "2b", "3", "4", "5"),
	lims = c(1, 2, 3, 4, 8, 18, 38, 68, 88))
data(testdata)
qry <- Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet"))
dta1 <- VegsoupData(qry)

as.binary(dta1)
as.numeric(dta1)
as.character(dta1)

summary(dta1)
MatrixFill(dta1)
Abbreviation(dta1)
AbundanceScale(dta1)
class(BraunBlanquetReduce(dta1))

dta <- VegsoupData(Vegsoup(species, sites, taxonomy, group = c(rep(1, 3), rep(2, 3)),
	scale = list(scale = "Braun-Blanquet")))
AprioriGrouping(dta)

data(bigtestdata)

qry2 <- Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet"))
dta2 <- VegsoupData(qry2)
	
prt <- VegsoupDataPartition(dta, k = 10, polish = FALSE)
Rectangles(prt, plot = TRUE, col = NA)

