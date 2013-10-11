library(vegsoup)
data(testdata)

new("Species", data = spc)
new("Sites", data = sts)
new("Taxonomy", data = txa)
new("SpeciesTaxonomy", species = species(spc), taxonomy = taxonomy(txa))
SpeciesTaxonomy(spc, txa)

dta <- Vegsoup(spc, sts, txa)
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")
dta <- Vegsoup(spc, sts, txa, coverscale = Coverscale("braun.blanquet"))
dta <- Vegsoup(spc, sts, txa,
	group = c(rep(1, 3), rep(2, 3)),
	coverscale = "braun.blanquet")

#	accessor methods for class slots
#	from class Vegsoup
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
fill(dta)

#	print, show and summary
head(dta)
head(dta, typeof = "logical")
head(dta, typeof = "numeric")
head(dta, typeof = "character")
head(dta, "si")
tail(dta, n=3)
summary(dta)
show(dta)
outlier(dta)
outlier(dta, thresh = 0.2)

#	dimnames and names
colnames(dta)
old.rownames <- rownames(dta)
rownames(dta) <- 1:nrow(dta)   # value = "integer"
row.names(dta) <- old.rownames # value = "character"
all.equal(rownames(Sites(dta)), rownames(dta))

names(dta)
names(dta)[1:2] <- names(dta)[1:2]

#	taxon abbreviations
head(taxon(dta))
head(SpeciesList(dta))
split.abbr(dta)
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
dta$X <- coordinates(dta)[, 1]
dta$Y <- coordinates(dta)[, 2]
coordinates(dta) <- ~X+Y
proj4string(dta)
proj4string(dta) <- CRS("+init=epsg:4326")
bbox(dta)

spTransform(dta, CRS("+init=epsg:3857"))
SpatialPointsVegsoup(dta)
SpatialPolygonsVegsoup(dta)

#	as.matrix
as.logical(dta)
as.logical(dta, mode = "q")
as.numeric(dta)
as.numeric(dta, mode = "q")
as.character(dta)
as.character(dta)
as.character(dta, mode = "q")
head(as.matrix(dta, "character", "q"))
tail(as.matrix(dta, "character", "q"))
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
as.numeric(dta)
decostand(dta)  <- c("hellinger", "standardize")
as.numeric(dta)
decostand(dta)  <- c("wisconsin")
as.numeric(dta)                  

#	subsetting
dta[1:3,]
dta[1,]
dim(dta[1:3,2:3])
Layers(dta[, grep("@sl", colnames(dta))])

#	subsample
rownames(SampleVegsoup(dta))

#	bind
s1 <- dta[1:2, ]
s2 <- dta[3:4, ]
s3 <- dta[5:6, ]

try(rownames(bind(s3, s1, s2)))

#	table methods
seriation(dta) # dca
seriation(dta, "hclust")
seriation(dta, "ward")
seriation(dta, "flexible")
seriation(dta, "packed")
as.data.frame(as.matrix(seriation(dta, "packed"), "character", "r"))

#	partitioning methods
prt <- VegsoupPartition(dta, k = 2)

fid <- Fidelity(prt, verbose = TRUE)
fid <- Fidelity(prt, verbose = TRUE, fast = TRUE)

head(prt)
getK(prt)
Partitioning(prt)
spread(prt)
contingency(prt)
constancy(prt)
shared(prt)

quantile(prt)[,,1] # min
quantile(prt, na.rm = FALSE)[,,2] # lower hinge
quantile(prt, coverscale = TRUE)[,,3] # median
prt[1,]
prt[,1:10]
Partition(prt, 1)
Optsil(prt)
Optindval(prt)
Partana(prt)
#silhouette(prt)
Disdiam(prt)
typical(prt)
#Murdoch(prt) 
Isamic(prt)
Tabdev(prt)

#	depreciated
FisherTest(prt)
Phi(prt)
Indval(prt)

data(bigtestdata)

dta <- Vegsoup(spc.big, sts.big, txa.big,
	coverscale = "braun.blanquet")
