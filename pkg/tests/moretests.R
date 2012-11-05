library(vegsoup)

data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
summary(dta)
MatrixFill(dta)
Abbreviation(dta)
AbundanceScale(dta)
class(BraunBlanquetReduce(dta))

dta <- VegsoupData(Vegsoup(species, sites, taxonomy, group = c(rep(1, 3), rep(2, 3)),
	scale = list(scale = "Braun-Blanquet")))
AprioriGrouping(dta)

data(bigtestdata)
dta <- VegsoupData(Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet")))
	
prt <- VegsoupDataPartition(dta, k = 10, polish = FALSE)
Rectangles(prt, plot = TRUE, col = NA)

