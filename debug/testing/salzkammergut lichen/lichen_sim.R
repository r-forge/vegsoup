#	check out from vegbase classes
spc <- species@pa
sts <- sites@raw

#	select well sampled substrate types
substrates <- table(sites@raw$substrate)
substrates <- substrates[substrates > 10]
substrates <- sapply(names(substrates),
	function(x) which(sts$substrate == x))
substrates <- sort(unlist(substrates))

sts <- sts[substrates,]
spc <- spc[substrates,]
spc <- spc[,apply(spc > 0, 2, sum) > 0]

foo <- permatfull(spc, hab = as.factor(sts$substrate),
	mtype = "prab", times = 10)
	
	
#check <- nestedchecker(spc)
#n0 <- nestedn0(spc)
#disc <- nesteddisc(spc)
#temp <- nestedtemp(spc)
#caeval <- function(x) decorana(x, ira=1)$evals
#foo <- oecosimu(spc<, caeval, "qusswap",
#	burnin=100, thin = 10)