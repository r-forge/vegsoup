library(vegsoup)
data(testdata)

head(species(new("Species", data = spc)))
head(sites(new("Sites", data = sts)))
head(taxonomy(new("Taxonomy", data = txa)))

dta <- VegsoupData(spc, sts, txa)
dta <- VegsoupData(spc, sts, txa, coverscale = "braun.blanquet")
dta <- VegsoupData(spc, sts, txa, coverscale = Coverscale("braun.blanquet"))
dta <- VegsoupData(spc, sts, txa,
	group = c(rep(1, 3), rep(2, 3)),
	coverscale = "braun.blanquet")

#	accessor methods for class slots
#	from class VegsoupData
Sites(dta)
Species(dta)
decostand(dta)
vegdist(dta)
AprioriGrouping(dta)
Taxonomy(dta)
SpatialPointsVegsoup(dta)
SpatialPolygonsVegsoup(dta)
Layers(dta)
coverscale(dta)

#	matrix dimensions
ncol(dta)
nrow(dta)
dim(dta)
ncell(dta)
MatrixFill(dta)

#	print, show and summary
head(dta)
head(dta, typeof = "logical")
head(dta, typeof = "numeric")
head(dta, typeof = "character")
head(dta, "si")
tail(dta, n=3)
summary(dta)
show(dta)

#	dimnames and names
colnames(dta)
rownames(dta)
names(dta)

#	taxon abbreviations
head(DecomposeNames(dta))
Abbreviation(dta)
abbr.layer(dta)
#	abundance scale

class(BraunBlanquetReduce(dta))
coverscale((BraunBlanquetReduce(dta)))
coverscale(dta)
#	both should fail!
#	coverscale(dta) <- "braun.blanquet2"
#	coverscale(dta) <- Coverscale("braun.blanquet")
#	assign what is already assigned
#	coverscale(dta) <- "braun.blanquet"

#	Layers
Layers(dta)
dim(dta)
dim(Layers(dta, collapse = c("hl", "sl", NA)))

#	spatial methods
coordinates(dta)
proj4string(dta)
bbox(dta)
spTransform(dta, CRS("+init=epsg:3857"))
SpatialPointsVegsoup(dta)
SpatialPolygonsVegsoup(dta)

#	as.matrix
as.logical(dta)
as.numeric(dta)
as.character(dta)
as.character(dta, mode = "R")
head(as.matrix(dta, "character", "r"))
head(t(as.character(dta)))
indices(dta)
indices(dta, "character")


#	row & column sums, means and richness
rowSums(dta)
rowSums(dta, typeof = "numeric")
rowMeans(dta)

colSums(dta)
colSums(dta, typeof = "numeric")
colMeans(dta)

richness(dta, "da")
richness(dta, "sa")

#	standardization
decostand(dta)  <- c("hellinger")
foo <- as.numeric(dta)
decostand(dta)  <- c("hellinger", "standardize")
foo <- as.numeric(dta)
decostand(dta)  <- c("wisconsin")
foo <- as.numeric(dta)                  

#	subsetting
dta[1:3,]
dta[1,]
dim(dta[1:3,2:3])
dim(Layers(dta[, grep("@sl", colnames(dta))]))

#	subsample
rownames(SampleVegsoup(dta))

#	rbind
s1 <- dta[1:2, ]
s2 <- dta[3:4, ]
s3 <- dta[5:6, ]

### broken!
try(rownames(rbind(s3, s1, s2)))

#	table methods
seriation(dta) # dca
seriation(dta, "hclust")
seriation(dta, "ward")
seriation(dta, "flexible")
seriation(dta, "packed")
as.data.frame(as.matrix(seriation(dta, "packed"), "character", mode = "r"))

#	partitioning methods
prt <- VegsoupDataPartition(dta, k = 2)

fid <- Fidelity(prt, verbose = TRUE)
#	inherited methods
head(prt)
getK(prt)
Partitioning(prt)
Spread(prt)
Contingency(prt)
Constancy(prt)
Shared(prt)
Fivenum(prt)[,,1] # min
Fivenum(prt, na.rm = FALSE)[,,2] # lower hinge
Fivenum(prt, recode = TRUE)[,,3] # median
prt[1,]
prt[,1:10]

Optsil(prt)
Optindval(prt)
Partana(prt)
try(Silhouette(prt))	#	broken
try(typal(prt))	#	broken
try(Murdoch(prt))	#	broken
Isamic(prt)
Tabdev(prt)

#	depreciated
FisherTest(prt)
Phi(prt)
Indval(prt)



data(bigtestdata)

dta <- VegsoupData(spc.big, sts.big, txa.big,
	coverscale = "braun.blanquet")
