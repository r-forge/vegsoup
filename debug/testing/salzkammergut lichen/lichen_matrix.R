library(vegan)
library(bipartite)

spc <- species[twig,]
sts <- sites[twig,]

#	drop very species poor plots
drop <- apply(spc@pa, 1, sum) > 0
spc <- spc[drop,]
sts <- sts[drop,]

#	raw/unpacked matrix
rm <- spc@pa

#	tabulate matrix sorted by decreasing site richness
#	and species frequency
i <- order(apply(rm > 0, 1, sum), decreasing = TRUE)
j <- order(apply(rm > 0, 2, sum), decreasing = TRUE)
pm <- rm[i,j]

#	matrix temperature
nt1 <- nestedness(pm, null.models = FALSE)
nt1$temp
nt2 <- nestedtemp(pm)
plot(nt2, kind = "inci")
#nestedchecker(res)
#nestedn0(res)
#nesteddisc(res)

#mod <- oecosimu(res, nestfun = "")