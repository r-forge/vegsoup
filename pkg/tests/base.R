require(rgdal)
require(vegsoup)

data(barmstein)
X <- barmstein

#	accessor methods for class slots
#	from class Vegsoup
species(X)
sites(X)
taxonomy(X)
decostand(X)
vegdist(X)
layers(X)
coverscale(X)
apriori(X)
SpatialPointsVegsoup(X)
SpatialPolygonsVegsoup(X)

#	matrix dimensions
dim(X)
ncol(X)
nrow(X)
ncell(X)
fill(X)

#	from class Species
x <- species(X)
species(x)

#	from class Sites
y <- sites(X)
#	is data.frame, hence not defined

#	from class Taxonomy
z <- taxonomy(X)
taxonomy(z)

#	from class SpeciesTaxonomy
xz <- SpeciesTaxonomy(x, z)
species(xz)
taxonomy(xz)

#	print, show and summary
head(X)
head(X, typeof = "logical")
head(X, typeof = "numeric")
head(X, typeof = "character")
head(X, "si")
tail(X, n=3)
summary(X)
show(X)
outlier(X)
outlier(X, thresh = 0.2)

#	dimnames and names
colnames(X)
old.rownames <- rownames(X)
rownames(X) <- 1:nrow(X)   # value = "integer"
row.names(X) <- old.rownames # value = "character"
all.equal(rownames(sites(X)), rownames(X))

names(X)
names(X)[1:2] <- names(X)[1:2]

#	taxon abbreviations
head(taxon(X))
head(taxalist(X))
(splitAbbr(X))

#	abundance scale
class(BraunBlanquetReduce(X))
coverscale((BraunBlanquetReduce(X)))
coverscale(X)

#	both way work
coverscale(X) <- "braun.blanquet2"
coverscale(X) <- Coverscale("braun.blanquet2")

#	assign what is already assigned, works
#coverscale(X) <- "braun.blanquet"

#	layers
layers(X)
dim(X)
dim(layers(X, collapse = c("hl", "sl", NA)))

#	spatial methods
coordinates(X)
X$X <- coordinates(X)[, 1]
X$Y <- coordinates(X)[, 2]
coordinates(X) <- ~X+Y
proj4string(X)

proj4string(X) <- CRS("+init=epsg:4326")
bbox(X)


spTransform(X, CRS("+init=epsg:3857"))
SpatialPointsVegsoup(X)
SpatialPolygonsVegsoup(X)

#	as.matrix
as.logical(X)
as.logical(X, mode = "q")
as.numeric(X)
as.numeric(X, mode = "q")
as.character(X)
as.character(X)
as.character(X, mode = "q")
head(as.matrix(X, "character", "q"))
tail(as.matrix(X, "character", "q"))
head(t(as.character(X)))
indices(X)
indices(X, "character")


#	row & column sums, means and richness
rowSums(X)
rowSums(X, typeof = "numeric")
rowMeans(X)

colSums(X)
colSums(X, typeof = "numeric")
colMeans(X)

richness(X, "da")
richness(X, "sa")

#	standardization
decostand(X)  <- c("hellinger")
as.numeric(X)
decostand(X)  <- c("hellinger", "standardize")
as.numeric(X)
decostand(X)  <- c("wisconsin")
as.numeric(X)				  

#	subsetting
X[1:3,]
X[1,]
dim(X[1:3,2:3])
layers(X[, grep("@sl", colnames(X))])

#	subsample
rownames(sample(X))

#	bind
s1 <- X[1:2, ]
s2 <- X[3:4, ]
s3 <- X[5:6, ]

try(rownames(bind(s3, s1, s2)))

data(barmstein)
dta <- barmstein

#	table methods
seriation(dta) # dca
seriation(dta, "hclust")
seriation(dta, "ward")
seriation(dta, "flexible")
seriation(dta, "packed")
as.data.frame(as.matrix(seriation(dta, "packed"), "character", "r"))

#	partitioning methods
prt <- VegsoupPartition(dta, k = 2)

fid <- fidelity(prt, verbose = TRUE)
fid <- fidelity(prt, verbose = TRUE, fast = TRUE)

ft1 <- FisherTest(prt, alternative = "two.sided")
ft2 <- getStat(fidelity(prt, method = "Fisher", alternative = "two.sided"))
all.equal(ft1, ft2)

head(prt)
getK(prt)
partitioning(prt)
spread(prt)
contingency(prt)
constancy(prt)
shared(prt)

quantile(prt)[,,1] # min
quantile(prt, na.rm = FALSE)[,,2] # lower hinge
quantile(prt, coverscale = TRUE)[,,3] # median

prt[1,]
prt[,1:10]

partition(prt, 1)

#	wait until optpart registers S3methods
#isamic(prt)
#typical(prt)
#optsil(prt)
#optindval(prt)
#partana(prt)
#silhouette(prt)
#disdiam(prt)
#Murdoch(prt)
#tabdev(prt)
#Indval(prt)

#	depreciated
FisherTest(prt)
Phi(prt)

#	test to compare nummerical stability of implementation
x <- coenoflex()
i.prt <- VegsoupPartition(x, k = 2)
require(indicspecies)
is <- strassoc(as.logical(i.prt),
		partitioning(i.prt),
		func = "IndVal.g")
head(is)
	
require(labdsv)
ld <- indval(as.logical(i.prt),
partitioning(i.prt))
head(sqrt(ld$indval))
	
decostand(i.prt) <- "pa"
vb <- fidelity(i.prt, method = "IndVal.g")	
head(sqrt(getStat(vb)))
