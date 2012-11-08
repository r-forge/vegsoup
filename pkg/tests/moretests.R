library(vegsoup)
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))

summary(dta)

as.binary(dta)
as.numeric(dta)
as.character(dta)
dta[1:3,]
dim(dta[1:3,2:3])

Layers(dta[, grep("@sl", names(dta))])
dim(foo)
as.character()
head(dta)
tail(dta, n=3L)
names(dta)
rownames(dta)
ncol(dta)
nrow(dta)
dim(dta)

rowSums(dta)
colSums(dta)

Layers(dta)
dim(Layers(dta, collapse = c("hl", "sl", NA)))

Richness(dta, "dataset")
Richness(dta, "sample")
t(as.character(dta))
as.data.frame(t(as.character(Arrange(dta, "packed"))))
Sites(Arrange(dta))
SampleVegsoup(dta)
MatrixFill(dta)
Abbreviation(dta)
AbundanceScale(dta)
class(BraunBlanquetReduce(dta))

Sites(dta)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy, group = c(rep(1, 3), rep(2, 3)),
	scale = list(scale = "Braun-Blanquet")))
AprioriGrouping(dta)

data(bigtestdata)

qry2 <- Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet"))
dta2 <- VegsoupData(qry2)
	
prt <- VegsoupDataPartition(dta, k = 10, polish = FALSE)
Rectangles(prt, plot = TRUE, col = NA)

