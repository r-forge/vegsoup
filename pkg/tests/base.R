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
Layers(X)
coverscale(X)
AprioriGrouping(X)
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
y <- Sites(X)
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
all.equal(rownames(Sites(X)), rownames(X))

names(X)
names(X)[1:2] <- names(X)[1:2]

#	taxon abbreviations
head(taxon(X))
head(SpeciesList(X))
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

#	Layers
Layers(X)
dim(X)
dim(Layers(X, collapse = c("hl", "sl", NA)))

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
Layers(X[, grep("@sl", colnames(X))])

#	subsample
rownames(SampleVegsoup(X))

#	bind
s1 <- X[1:2, ]
s2 <- X[3:4, ]
s3 <- X[5:6, ]

try(rownames(bind(s3, s1, s2)))