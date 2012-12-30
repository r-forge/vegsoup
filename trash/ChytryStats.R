#sav, gmv, group = NULL

xm <- function (N, Np, n, np) { 
	res <- matrix(c(
		np,
		Np - np,
		n - np,
		N - Np - n + np), 2, 2)	
	res[is.nan(res)] <- 0
    return(res)	
}

# f(e)_i
Em <- function (N, Np, n, np) { 
	res <- matrix(c(
		n * Np / N,
		(N - n) * Np / N,
		n * (N - Np) / N,
		(N -n) * (N - Np) / N), 2, 2)	
	res[is.nan(res)] <- 0
    return(res)
}

obj = prt

N <- nrow(obj)						# number of plots
n.i <- colSums(as.logical(obj))			# species frequencies
Npi <- table(Partitioning(obj))	# number of plots in partition
npi <- Contingency(obj)			# number of occurences in partition

G <- chi <- u.hyp <- u.binB <- u.binA <- npi

for (i in 1:ncol(obj)) { # loop over species
	n <- n.i[i]	# 17					# n	
    for (j in 1:length(Npi)) {		# loop over partitions
		Np <- Npi[j] # 18
		np <- npi[i,j] # 0
		#	u variants	
		u <- np - (n * (Np / N))
		#	with Bruelheide's correction	
		if (np - n * Np / N > 0.5) u <- u - 0.5
		if (np - n * Np / N < -0.5) u <- u + 0.5
		if (abs(np - n * Np / N) <= 0.5) u <- 0 # ?

		s.hyp <- sqrt((n * Np * (N -n) * (N - Np)) / (N^2 * (N - 1)))
		s.binB <- sqrt(n * (Np / N) * (1 - Np / N))
		s.binA <- sqrt(Np * (n / N) * (1 - n / N))
    	u.hyp[i,j] <- u / s.hyp
    	u.binB[i,j] <- u / s.binB
    	u.binA[i,j] <- u / s.binA
    	
#    	chi <- (N * (N * np - n * Np)^2) / (n * Np * (N -n) * (N - Np))
		#	X^2 with Ytes correction
    	chi[i,j] <- (N * (abs(N * np - n * Np) - (N / 2))^2) / (n * Np * (N - n) * (N - Np))
		#	G with williams correction
		x <- xm(N, Np, n, np)
		E <- Em(N, Np, n, np)
		g <- 0
		for (ii in 1:2){
			for (jj in 1:2){
				if (x[ii,jj] != 0) g <- g + x[ii,jj] * log(x[ii,jj] / E[ii,jj])
			}
		}
		g <- 2 * g
		q1 <- 1 + (1 / (6 * N))
		q2 <- (N / n) + (N / (N - n)) - 1
		q3 <- (N / Np) + (N / (N - Np)) - 1
		q <- q1 * q2 * q3
		
		G[i,j] <- g / q
#		u.binB <- u.hyp * sqrt((N - n) / (N -1)) # Chytry et al 2002 eq (7)
#		u.binA <- u.hyp * sqrt((N - Np) / (N - 1) # Chytry et al 2002 eq (8)
    }
}


