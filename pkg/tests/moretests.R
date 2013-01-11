library(vegsoup)
data(testdata)        
qry <- Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet"))
dta <- VegsoupData(qry)

summary(dta)

dim(dta)
ncol(dta)
nrow(dta)
ncell(dta)

head(dta)
head(dta, mode = "logical")
head(dta, mode = "numeric")
head(dta, mode = "character")
head(dta, "si")
tail(dta, n=3)
colnames(dta)
rownames(dta)
ncol(dta)
nrow(dta)
dim(dta)

rowSums(dta)
colSums(dta)

Layers(dta)
dim(Layers(dta, collapse = c("hl", "sl", NA)))

rowSums(dta)
rowSums(dta, mode = "numeric")

colSums(dta)
colSums(dta, mode = "numeric")

as.logical(dta)
as.numeric(dta)
as.character(dta)

decostand(dta)  <- c("hellinger")
as.numeric(dta)
decostand(dta)  <- c("hellinger", "standardize")
as.numeric(dta)
decostand(dta)  <- c("wisconsin")
as.numeric(dta)                  

dta[1:3,]
dta[1,]
dim(dta[1:3,2:3])

s1 <- dta[1:2, ]
s2 <- dta[3:4, ]
s3 <- dta[5:6, ]

res <- rbind(s3, s1, s2)
rownames(res)
SpatialPointsVegsoup(res)
SpatialPolygonsVegsoup(res)

Layers(dta[, grep("@sl", colnames(dta))])

richness(dta, "da")
richness(dta, "sa")
t(as.character(dta))

Arrange(dta)
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
