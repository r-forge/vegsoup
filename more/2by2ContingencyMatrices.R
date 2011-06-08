N <- nrow(obj)						# number of plots
n.i <- colSums(getBin(obj))			# species frequencies
N_pi <- table(Partitioning(obj))	# number of plots in partition
n_pi <- Contingency(obj)			# number of occurences in partition

#	natation follows Bruelheide (1995, 2000)
#	cited in Chytry et al 2002:80

#	N: number of plots in the data set
#	N_p: number of plots in partition
#	n: number of occurences in the data set
#	n_p: number of occurences in parttion 

ObservedFreqencyTable <- function (N, N_p, n, n_p) { # f(o)_i
	res <- matrix(c(
		n_p,			# n_pi[i,j] for the the i species
		N_p - n_p,
		n - n_p,
		N - N_p - n + n_p), 2, 2)	
	res[is.nan(res)] <- 0			# f(o)_i
    return(res)	
}

ExpectedFreqencyTable <- function (N, N_p, n, n_p) { # f(e)_i
	res <- matrix(c(
		n * N_p / N,
		(N - n) * N_p / N,
		n * (N - N_p) / N,
		(N -n) * (N - N_p) / N), 2, 2)	
	res[is.nan(res)] <- 0
    return(res)
}

for (i in 1:ncol(obj)) { # loop over species
	n <- n.i[i]						# n	
    for (j in 1:length(N_pi)) {		# loop over partitions
		N_p <- N_pi[j]				# N_p
		n_p <- n_pi[i,j]    	
    foi <- ObservedFreqencyTable(N, N_p, n, n_p)
    fei <- ExpectedFreqencyTable(N, N_p, n, n_p)
    res1 <- 2 * sum(foi * log(foi / fei), na.rm = T)
    res2 <- g.statistic(fei, correction = "none")
    }
}	



#foo <- cbind(i = rep(1:ncol(obj), each = length(N_pi)),
#j = rep(1:length(N_pi), ncol(obj)))

