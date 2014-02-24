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
Optindval(prt)
Isamic(prt)
typical(prt)

# broken dispatch for optpart functions?
#optsil(prt)
#partana(prt)
#silhouette(prt)
#disdiam(prt)
#Murdoch(prt)
#tabdev(prt)
#Indval(prt)

#	depreciated
FisherTest(prt)
Phi(prt)

