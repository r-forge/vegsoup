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

s1 <- dta[1:2, ]
s2 <- dta[3:4, ]
s3 <- dta[5:6, ]

res <- rbind(s3, s1, s2)


Layers(dta[, grep("@sl", names(dta))])

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
Sites(dta)
Sites(Arrange(dta))
SampleVegsoup(dta)
MatrixFill(dta)
Abbreviation(dta)
AbundanceScale(dta)
class(BraunBlanquetReduce(dta))

dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	group = c(rep(1, 3), rep(2, 3)),
	scale = list(scale = "Braun-Blanquet")))
AprioriGrouping(dta)

data(bigtestdata)

qry2 <- Vegsoup(species.big, sites.big, taxonomy.big,
	scale = list(scale = "Braun-Blanquet"))
dta2 <- VegsoupData(qry2)
	
prt <- VegsoupDataPartition(dta2, k = 3, polish = FALSE)
Rectangles(prt, plot = TRUE, col = NA)

fid <- Fidelity(prt, "TCR", group = 4, verbose = T)
head(fid@stat)
head(fid@fisher.test)
