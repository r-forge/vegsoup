#	test to compare nummerical stability of implementation
is <- strassoc(as.binary(i.prt),
	Partitioning(i.prt),
	func = "IndVal.g")
head(is)

vb <- Fidelity(i.prt, method = "IndVal.g")	
head(getStat(vb))

ld <- indval(getBin(i.prt),
	Partitioning(i.prt))
head(sqrt(ld$indval))