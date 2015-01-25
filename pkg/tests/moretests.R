require(vegsoup)

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

#	data(sim100)
#	i.prt <- VegsoupPartition(sim100, k = 2)
#	require(indicspecies)
#	is <- strassoc(as.logical(i.prt),
#		partitioning(i.prt),
#		func = "IndVal.g")
#	head(is)
	
#	require(labdsv)
#	ld <- indval(as.logical(i.prt),
#		partitioning(i.prt))
#	head(sqrt(ld$indval))
	
#	decostand(i.prt) <- "pa"
#	vb <- fidelity(i.prt, method = "IndVal.g")	
#	head(sqrt(getStat(vb)))
