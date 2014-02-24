require(vegsoup)

data(barmstein)
dta <- barmstein
dta

#	accessor methods for class slots
#	from class Vegsoup
species(dta)
Sites(dta)
decostand(dta)
vegdist(dta)
AprioriGrouping(dta)
Taxonomy(dta)
SpatialPointsVegsoup(dta)
SpatialPolygonsVegsoup(dta)
Layers(dta)
coverscale(dta)

#	matrix dimensions
dim(dta)
ncol(dta)
nrow(dta)
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
(splitAbbr(dta))

#	abundance scale
class(BraunBlanquetReduce(dta))
coverscale((BraunBlanquetReduce(dta)))
coverscale(dta)
#	both should fail!
#	coverscale(dta) <- "braun.blanquet2"
#	coverscale(dta) <- Coverscale("braun.blanquet2")

#	assign what is already assigned, works
coverscale(dta) <- "braun.blanquet"

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