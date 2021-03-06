#	adapted and extended from strassoc.R by Miquel De C\'{a}ceres in package(indicspecies)

#	Fidelity measures

#	to do: add column for indicator value, high priority!
".fidelityVegsoupPartition" <- function (obj, method = "r.g", group = NULL, nboot = 0, alpha = 0.05, c = 1, alternative = "greater", fast = FALSE, verbose = TRUE, ...) {

	#	OptimClass (Tichy et al. 2010)
	#	tests the right-tailed hypothesis (alternative = "greater") of a Fisher test
	
	#	gmv: group membership vector (clusters)
	#	sav: species abundance vector

	if (getK(obj) < 2) {
		if (verbose)
			cat("results maybe meaningless with k = ", getK(obj))
	}
	
	if (method %in% c("r.ind", "r.ind.g", "s.ind", "s.ind.g", "TCR")) {
		if (!is.null(decostand(obj)) & method == "TCR") {
			message("TCR is not defined for standardized data!",
				"\nset decostand(obj) <- NULL")
			#	dont recompute partitioning
			obj@decostand <- new("decostand", method = NULL)
			#	decostand(obj) <- NULL
		}
		if (verbose) cat("individual based index")
		r.ind <- TRUE #	Fisher test needs binary matrix
		X <- as.numeric(obj)
	}
	else {
		if (verbose) cat("presence/absence based index\n")
		X <- as.logical(obj)
		r.ind <- FALSE # Fisher can use matrix X
	}

	cluster <- as.factor(partitioning(obj))
	
	if (!is.null(group)) {
		if (group > getK(obj)) stop("group must be within getK(obj)")
	}

	#	functions that compute diagnostic values by row
	#	Indval 1 and 2
	IndVal1 <- function (sav, gmv, group = NULL, ...) {
		gmv <- as.factor(gmv)
		means <- vector("numeric", nlevels(gmv))
		
		for (i in 1:nlevels(gmv)) {
			means[i] <- mean(sav[gmv == levels(gmv)[i]])
			if (is.na(means[i])) means[i] <- 0
		}
		if (is.null(group)) {	# indval1 for all groups
			indvals <- data.frame(matrix(0, nrow = nlevels(gmv), ncol = 3))
			row.names(indvals) <- levels(gmv)
			names(indvals) <- c("A.g", "B", "IndVal.g")
			
			for (i in 1:nlevels(gmv)) {
				indvals[i,1] <- ifelse(sum(means) > 0,
					means[i] / sum(means), 0)
				indvals[i,2] <- ifelse(sum(gmv == levels(gmv)[i]) > 0,
					sum((gmv == levels(gmv)[i]) & (sav > 0)) / sum(gmv == levels(gmv)[i]), 0)
				indvals[i,3] <- sqrt(indvals[i,1]*indvals[i,2])
			}
		}
		else {	# indval1 for one group
			indvals <- data.frame(matrix(0, nrow = 1, ncol = 3))
			row.names(indvals) <- c(group)
			names(indvals) <- c("A.g", "B", "IndVal.g")
			m <- mean(sav[gmv == group])
			if (is.na(m)) m <- 0
			indvals[1,1] <- ifelse(sum(means) > 0,
				m / sum(means), 0)
			indvals[1,2] <- ifelse(sum(gmv == group) > 0,
				sum((gmv == group) & (sav > 0)) / sum(gmv == group), 0)
			indvals[1,3] <- sqrt(indvals[1, 1] * indvals[1, 2])
		}
		return (indvals)
	}
	
	IndVal2 <- function(sav, gmv, group = NULL, ...) {
		gmv <- as.factor(gmv)
		sums <- vector("numeric", nlevels(gmv))
		
		for (i in 1:nlevels(gmv))
			sums[i] <- sum(sav[gmv == levels(gmv)[i]])

		# indval2 for all groups
		if (is.null(group)) { 
			indvals <- data.frame(matrix(0, nrow = nlevels(gmv), ncol = 3))
			row.names(indvals) <- levels(gmv)
			
			for (i in 1:nlevels(gmv)) {
				indvals[i, 1] <- ifelse(sum(sums) > 0,
					sums[i] / sum(sums), 0)
				indvals[i, 2] <- ifelse(sum(gmv == levels(gmv)[i]) > 0,
					sum((gmv == levels(gmv)[i]) & (sav > 0)) / sum(gmv == levels(gmv)[i]), 0)
				indvals[i, 3] <- sqrt(indvals[i, 1] * indvals[i, 2])
			}
		}
		# indval2 for one group
		else {
			indvals <- data.frame(matrix(0, nrow = 1, ncol = 3))
			row.names(indvals) <- c(group)
			sg <- sum(sav[gmv == group])
			indvals[1, 1] <- ifelse(sum(sums) > 0,
				sg / sum(sums), 0)
			indvals[1, 2] <- ifelse(sum(gmv == group) > 0,
				sum((gmv == group) & (sav > 0)) / sum(gmv == group), 0)
			indvals[1, 3] <- sqrt(indvals[1] * indvals[2])
		}
		names(indvals) <- c("A", "B", "IndVal")
		return(indvals)
	}
		
	#	phi (point biserial correlation)
	r.s <- function (sav, gmv, group = NULL, ...) {
		gmv <- as.factor(gmv)
		if (is.null(group)) {
			r <- vector("numeric", nlevels(gmv))
			for (i in 1:nlevels(gmv)) {
				r[i] <- cor(sav, gmv == levels(gmv)[i])
				if (is.na(r[i])) r[i] <- 0
			}
		}
		else {
			r <- cor(sav, ifelse(gmv == group, 1, 0))
			if (is.na(r)) r <- 0
		}
		return(r)
	}
	
	#	phi, group equalised
	r.g <- function (sav, gmv, group = NULL, ...) {
		gmv <- as.factor(gmv)
		npm <- vector("numeric", nlevels(gmv))
		k <- nlevels(gmv)
		N <- length(sav)
		nm <- l2m <- npmg <- 0
		C <- 1 / k
	
		for (i in 1:k) {
			Np <- sum(gmv == levels(gmv)[i]) # N_p
			np <- sum(sav * (gmv == levels(gmv)[i])) # n_p
			n2p <- sum((sav^2) * (gmv == levels(gmv)[i]))
			npm[i] <- (np / Np) * (N * C)
			if (Np == 0) npm[i] <- 0
			nm <- nm + npm[i]
			a <- (n2p  /Np) * (N * C)
			if (Np == 0) a <- 0
			l2m <- l2m + a	   
			if (!is.null(group)) if (levels(gmv)[i] == group) npmg <- npm[i]
		}
	
		Nm <- k * N * C
		s2 <- (Nm * l2m) - (nm^2)
		g2 <- C * (1 - C)
		if (is.null(group)) {
			r <- vector("numeric", nlevels(gmv))
			for (i in 1:k) {
				num <- npm[i] - (nm * C)
				r[i] <- num / sqrt(s2 * g2)
				if (num == 0) r[i] <- 0
				if (is.nan(r[i])) cat(l2m, " " , nm, "\n")
			}
		}
		else {
			num <- npmg-(nm*C)
			r <- num/sqrt(s2*g2)
			if (num == 0) r <- 0
		}
		return(r)
	}
	
	#	phi individual based
	r.ind.s <- function (sav, gmv, group = NULL, c = 1, ...) {
		gmv <- as.factor(gmv)
		N <- length(sav)
		asp <- sum(sav)
		if (is.null(group)) {
			r <- vector("numeric", nlevels(gmv))
			for (i in 1:nlevels(gmv)) {
				ni <- sum(gmv == levels(gmv)[i])
				aspi <- sum(sav * (gmv == levels(gmv)[i]))
				num <- (N * aspi) - (asp * ni)
				s2 <- N * c * asp - asp^2
				g2 <- N * ni - ni^2
				r[i] <- num / sqrt(s2 * g2)
				if (num == 0) r[i] <- 0
			}
		}
		else {
			ni <- sum(gmv == group)
			aspi <- sum(sav * (gmv == group))
			num <- (N * aspi) - (asp * ni)
			s2 <- N * c * asp - asp^2
			g2 <- N * ni - ni^2
			r <- num / sqrt(s2 * g2)
			if (num == 0) r <- 0
		}
		return(r)
	}
	
	#	phi individual based, group equalised
	r.ind.g <- function (sav, gmv, group = NULL, c = 1, ...) {
		gmv <- as.factor(gmv)
		aspig <- vector("numeric", nlevels(gmv))
		k <- nlevels(gmv)
		N <- length(sav)
		aspg <- 0
		nig <- N / k
	
		for (i in 1:k) {
			aspig[i] <- (N / k) * (sum(sav * (gmv == levels(gmv)[i])) / sum(gmv == levels(gmv)[i]))
			aspg <- aspg + aspig[i]
			if (!is.null(group)) {
				if (group == levels(gmv)[i]) igroup <- i
			}
		}
	
		if (is.null(group)) {
			r <- vector("numeric", nlevels(gmv))
			for (i in 1:k) {
				num <- (N * aspig[i]) - (aspg * nig)
				s2 <- N * c * aspg - aspg^2
				g2 <- N * nig - nig^2
				r[i] <- num / sqrt(s2 * g2)
				if (num == 0) r[i] <- 0
			}	 
		}
		else {
			num=(N*aspig[igroup])-(aspg*nig)
			s2 <- N * c * aspg - aspg^2
			g2 <- N * nig - nig^2
			r <- num / sqrt(s2 * g2)
			if (num == 0) r <- 0
		}
		return(r)
	}
	
	#	square root of Indval A * Indval B
	s.ind.s <- function (sav, gmv, group = NULL, c = 1, ...) {
		gmv <- as.factor(gmv)
		N <- length(sav)
		asp <- sum(sav)
		if (is.null(group)) {
			r <- vector("numeric", nlevels(gmv))
			for (i in 1:nlevels(gmv)) {
				ni <- sum(gmv == levels(gmv)[i])
				aspi <- sum(sav * (gmv == levels(gmv)[i]))
				r[i] <- aspi / sqrt(asp * c * ni)
				if (aspi == 0) r[i] <- 0
			}
		}
		else {
			ni <- sum(gmv == group)
			aspi <- sum(sav * (gmv == group))
			r <- aspi / sqrt(asp * c * ni)
			if (aspi == 0) r <- 0
		}
		return(r)
	}
	
	#	square root of Indval A.g * Indval B, group equalised
	s.ind.g <- function (sav, gmv, group = NULL, c = 1, ...) {
		gmv <- as.factor(gmv)
		N <- length(sav)
		k <- nlevels(gmv)
		asp <- 0
		
		for (i in 1:k) {
		asp <- asp + sum(sav * (gmv == levels(gmv)[i])) / sum(gmv == levels(gmv)[i])
		}	 
		
		if (is.null(group)) {
			r <- vector("numeric", nlevels(gmv))
			for (i in 1:k) {
				ni <- sum(gmv == levels(gmv)[i])
				aspi <- sum(sav * (gmv == levels(gmv)[i]))
				r[i] <- sqrt(((aspi / ni) * aspi) / (asp * c * ni))
				if (aspi == 0) r[i] <- 0
			} 
		}
		else {
			ni <- sum(gmv == group)
			aspi <- sum(sav * (gmv == group))
			#	r <- sqrt(((aspi / ni) * aspi) / (asp * c * ni))
			r <- sqrt(((aspi / ni)) / (asp * c))
			if (aspi == 0) r <- 0
		}
		return(r)
	}
	
	#	cosine
	cos.s <- function (sav, gmv, group = NULL, ...) {
		gmv <-  as.factor(gmv)
		r <- vector("numeric", nlevels(gmv))
		x1 <- as.numeric(sav)
		l1 <- sqrt(sum(x1 * x1))
		if (is.null(group)) {
			k <- nlevels(gmv)
			for (i in 1:k) {
				x2 <- ifelse(gmv == levels(gmv)[i], 1, 0)
				l2 <- sqrt(sum(x2 * x2))
				r[i] <- (sum(x1 * x2) / (l1 * l2))
				if (is.na(r[i])) r[i] <- 0 
			}
		}
		else {
			x2 <- ifelse(gmv == group, 1, 0)
			l2 <- sqrt(sum(x2 * x2))
			r <- (sum(x1 * x2) / (l1 * l2))
			if (is.na(r)) r <- 0
		}
		return(r)
	}
	
	#	cosine, group eqalised
	cos.g <- function (sav, gmv, group = NULL, ...) {
		gmv <- as.factor(gmv)
		s <- 0
		k <- nlevels(gmv)
		if (is.null(group)) {
			n <- a <- r <- vector("numeric", nlevels(gmv))
			
			for (i in 1:k) {
				n[i] <- sum(gmv == levels(gmv)[i])
				a[i] <- sum(sav * (gmv == levels(gmv)[i]))
				a2 <- sum((sav^2) * (gmv == levels(gmv)[i]))
				s <- s + (a2 / n[i])
			}
			
			for (i in 1:k) r[i] <- (a[i] / n[i]) / sqrt(s)
		}
		else {
			for (i in 1:k) {
				n <- sum(gmv == levels(gmv)[i])
				a2 <- sum((sav^2) * (gmv == levels(gmv)[i]))
				s <- s + (a2 / n)
			}
			
		r <- (sum(sav * (gmv == group)) / sum(gmv == group)) / sqrt(s)
		#	if (is.na(r)) r <- 0 
		}
		return(r)
	}
	
	#	Bruelheide's corrected u value
	u.s <- function (sav, gmv, group = NULL, ...) {
	
		r <- data.frame(matrix(0, nrow = nlevels(gmv), ncol = 3))
		row.names(r) <- levels(gmv)
		names(r) <- c("u", "u.binB", "u.binA")
		N <- length(sav)
		for (i in 1:nlevels(gmv)) { # loop over partitions
	
			n  <- sum(sav)	
			Np <- sum(gmv == levels(gmv)[i])
			np <- sum(sav * (gmv == levels(gmv)[i]))
			u <- np - (n * (Np / N))
			#	Bruelheide's correction	
			if (np - n * Np / N > 0.5) u <- u - 0.5
			if (np - n * Np / N < -0.5) u <- u + 0.5
			if (abs(np - n * Np / N) <= 0.5) u <- 0 # ?
			s.hyp <- sqrt((n * Np * (N -n) * (N - Np)) / (N^2 * (N - 1)))
			s.binB <- sqrt(n * (Np / N) * (1 - Np / N))
			s.binA <- sqrt(Np * (n / N) * (1 - n / N))
			r[i, 1] <- u / s.hyp
			r[i, 2] <- u / s.binB
			r[i, 3] <- u / s.binA
		}
		if (!is.null(group)) r <- r[group,]
		return(r)
	}
	
	#	G statsitic
	g.s <- function (sav, gmv, group = NULL, ...) {

	#	observed frequencies
		xm <- function (N, Np, n, np) { 
			res <- matrix(c(
				np,
				Np - np,
				n - np,
				N - Np - n + np), 2, 2)	
			res[is.nan(res)] <- 0
			return(res)	
		}
	
	#	expected frequencies
		Em <- function (N, Np, n, np) { 
			res <- matrix(c(
				n * Np / N,
				(N - n) * Np / N,
				n * (N - Np) / N,
				(N - n) * (N - Np) / N), 2, 2)	
			res[is.nan(res)] <- 0
			return(res)
		}
		N <- length(sav)
		r <- vector("numeric", nlevels(gmv))
	
		for (i in 1:nlevels(gmv)) { # loop over partitions
			n <- sum(sav)
			Np <- sum(gmv == levels(gmv)[i])
			np <- sum(sav * (gmv == levels(gmv)[i]))
			x <- xm(N, Np, n, np)
			E <- Em(N, Np, n, np)
			g <- 0
			for (ii in 1:2){
				for (jj in 1:2){
					if (x[ii, jj] != 0) g <- g + x[ii, jj] * log(x[ii, jj] / E[ii, jj])
				}
			}
			g <- 2 * g
			q1 <- 1 + (1 / (6 * N))
			q2 <- (N / n) + (N / (N - n)) - 1
			q3 <- (N / Np) + (N / (N - Np)) - 1
			q <- q1 * q2 * q3
			
			r[i] <- g / q
		}
		if (!is.null(group)) r <- r[group]
		#	pval <- 1 - pchisq(r, df = 1)
		return(r)
	}
	
	#	chi-square statistic with Yates correction
	chi.s <- function (sav, gmv, group = NULL, ...) {
	
		r <- vector("numeric", nlevels(gmv))
		N <- length(sav)
		for (i in 1:nlevels(gmv)) { # loop over partitions
			n  <- sum(sav)	
			Np <- sum(gmv == levels(gmv)[i])
			np <- sum(sav * (gmv == levels(gmv)[i]))
			#	eq (13) Chytry et al 2002:82
			r[i] <- (N * (abs(N * np - n * Np) - (N / 2))^2) / (n * Np * (N - n) * (N - Np))
		}
		if (!is.null(group)) r <- r[group]
		return(r)
	}
	
	#	Fisher Test, apapted from isotab.R (package 'isopam')
	Fisher.s <-  function (sav, gmv, group = NULL, alternative = alternative) {
	
		alternative <-  match.arg(as.character(alternative), c("greater","less","two.sided"))
	
		FisherPval <- function (x) {
			p <- NULL
			m <- sum(x[, 1])
			n <- sum(x[, 2])
			k <- sum(x[1, ])
			x <- x[1, 1]
			lo <- max(0, k - n)
			hi <- min(k, m)
			support <- lo:hi
			logdc <- dhyper(support, m, n, k, log = TRUE)
	
			dnhyper <- function (ncp) {
				d <- logdc + log(ncp) * support
				d <- exp(d - max(d))
				d / sum(d)
			}
			
			pnhyper <- function (q, ncp = 1, upper.tail = FALSE) {
				if (ncp == 1) {
					if (upper.tail)
						return(phyper(x - 1, m, n, k, lower.tail = FALSE))
					else
						return(phyper(x, m, n, k))
				}
				if (ncp == 0) {
					if (upper.tail)
						return(as.numeric(q <= lo))
					else
						return(as.numeric(q >= lo))
				}
				if (ncp == Inf) {
					if (upper.tail)
						return(as.numeric(q <= hi))
					else
						return(as.numeric(q >= hi))	
				}
				d <- dnhyper(ncp)
				if (upper.tail)
					sum(d[support >= q])
				else
					sum(d[support <= q])
			}
	
		p <- switch(alternative,
			less = pnhyper(x, 1),
			greater = pnhyper(x, 1, upper.tail = TRUE),
			two.sided = {
					relErr <- 1 + 10^(-7)
					d <- dnhyper(1)
					sum(d[d <= d[x - lo + 1] * relErr])
				})
		return(p)
	}	
	
	#	observed frequencies
		xm <- function (N, Np, n, np) {
			res <- matrix(c(
				np,
				Np - np,
				n - np,
				N - Np - n + np), 2, 2)
			res[is.nan(res)] <- 0
			return(res)	
		}
	
		r <- vector("numeric", nlevels(gmv))
		N <- length(sav)
		
		for (i in 1:nlevels(gmv)) { # loop over partitions
			n  <- sum(sav)
			Np <- sum(gmv == levels(gmv)[i])
			np <- sum(sav * (gmv == levels(gmv)[i]))
			r[i] <- FisherPval(xm(N, Np, n, np))
		}
		if (!is.null(group)) r <- r[group]
		return(r)
	}
	
	#	constancy ratio as defined in Willner et al. 2009
	cr.s <- function (sav, gmv, group = NULL, ...) {
		sav[sav > 0] <- 1
		cnst <- table(sav, gmv)
		cnst[1,] <- colSums(cnst)
		cnst <- round(apply(cnst, 2,
			function (x) x[2] / x[1]) * 100, 0)
		N <- length(sav)
		#	avoid ratios greater than 100
		cnst[cnst > 0 & cnst < 1] <- 1
			r <- vector("numeric", nlevels(gmv))
			#	avoid high values for species with low constancy
			if (!all(cnst <= 20)) {
				for (i in 1:nlevels(gmv)) {
						r[i] <- cnst[i] / max(cnst)
				}
			}
		if (!is.null(group)) r <- r[group]
		return(r)
	}
	
	#	total cover ratio as defined in Willner et al. 2009
	tcr.ind.s <- function (sav, gmv, group = NULL, ...) {
		#	avoid ratios greater than 100
		#	cnst[cnst > 0 & cnst < 1] <- 1
		#	avoid high values for species with low cover
	#		if (!max(sav) < 1) {
				r <- vapply(1:nlevels(gmv),
					FUN = function (x) mean(sav[gmv == levels(gmv)[x]]) / max(sav),
					FUN.VALUE = numeric(1))
	#		}
		if (!is.null(group)) r <- r[group]
		return(r)
	}

	#	method functions
	fidelity.method <- function (sav, gmv, method = "r", group = NULL, ...) {
			 if (method == "r")			a <- r.s(sav, gmv, group)
		else if (method == "r.g")		a <- r.g(sav, gmv, group)
		else if (method == "cos")		a <- cos.s(sav, gmv, group)
		else if (method == "cos.g")		a <- cos.g(sav, gmv, group)
		else if (method == "r.ind")		a <- r.ind.s(sav, gmv, group, c)
		else if (method == "r.ind.g")	a <- r.ind.g(sav, gmv, group, c)
		else if (method == "s.ind")		a <- s.ind.s(sav, gmv, group, c)
		else if (method == "s.ind.g")	a <- s.ind.g(sav, gmv, group, c)
		else if (method == "IndVal.g")	a <- IndVal1(sav, gmv, group)[, 3]
		else if (method == "IndVal")	a <- IndVal2(sav, gmv, group)[, 3]
		else if (method == "A.g")		a <- IndVal1(sav, gmv, group)[, 1]
		else if (method == "A")			a <- IndVal2(sav, gmv, group)[, 1]
		else if (method == "B")			a <- IndVal2(sav, gmv, group)[, 2]
		else if (method == "u.hyp")		a <- u.s(sav, gmv, group)[, 1]
		else if (method == "u.binB")	a <- u.s(sav, gmv, group)[, 2]
		else if (method == "u.binA")	a <- u.s(sav, gmv, group)[, 3]
		else if (method == "g")			a <- g.s(sav, gmv, group)
		else if (method == "chi")		a <- chi.s(sav, gmv, group)
		else if (method == "Fisher")	a <- Fisher.s(sav, gmv, group, ...)
		else if (method == "CR")		a <- cr.s(sav, gmv, group)
		else if (method == "TCR")		a <- tcr.ind.s(sav, gmv, group)
		
		return(a)
	}
	
	#	verbose method names
			 if (method == "r")			method.name <- "phi (point-biserial correlation coefficient)"
		else if (method == "r.g")		method.name <- "phi, group equalised"
		else if (method == "cos")		method.name <- "cosine (Ochiai index)" 
		else if (method == "cos.g")		method.name <- "cosine (Ochiai index), group equalised"
		else if (method == "r.ind")		method.name <- "phi individual based"
		else if (method == "r.ind.g")	method.name <- "phi individual based, group equalised"
		else if (method == "s.ind")		method.name <- "sqrt of Indval A times Indval B"
		else if (method == "s.ind.g")	method.name <- "sqrt of Indval A.g * Indval B, group equalised"
		else if (method == "IndVal.g")	method.name <- "Indval, group equalised"
		else if (method == "IndVal")	method.name <- "Indval"
		else if (method == "A.g")		method.name <- "Indval A, group equalised"
		else if (method == "A")			method.name <- "Indval A"
		else if (method == "B")			method.name <- "Indval B"
		else if (method == "u.hyp")		method.name <- "Bruelheide's corrected u value"
		else if (method == "u.binB")	method.name <- "Bruelheide's corrected u value uBinB"
		else if (method == "u.binA")	method.name <- "Bruelheide's corrected u value uBinA"
		else if (method == "g")			method.name <- "G statistic"
		else if (method == "chi")		method.name <- "chi-square statistic with Yates correction"
		else if (method == "Fisher")	method.name <- "Fisher test"
		else if (method == "CR")		method.name <- "constancy ratio"
		else if (method == "TCR")		method.name <- "total cover ratio"
	
	if (sum(is.na(cluster)) > 0) stop("Cannot deal with NA values. Please Remove and run again.")
	if (sum(is.na(X)) > 0) stop("Cannot deal with NA values. Please Remove and run again.")
	
	#	init multicore if active
	if (fast) {
		message("fork multicore process on ", parallel::detectCores(), " cores")
	}	
	#	create result object for fidelity measure
	nsps <- dim(X)[2]	# dim(obj)[2] 
	nsts <- dim(X)[1]	# dim(obj)[1] 
	
	ngps <- nlevels(cluster)
	if (!is.null(group)) {
		ngps <- 1
	}
	
	cpu.time.dm <- system.time({
	
		if (verbose) {
			pbapply::pboptions(char = ".")
		}	
		if (fast) {
			dm <- mclapply(as.data.frame(X),
				function (x, ...) fidelity.method(
				x, cluster, method, group, ...))
			dm <- matrix(unlist(dm), ncol = nlevels(cluster), nrow = length(dm),
				dimnames = list(names(dm), levels(cluster)), byrow = TRUE)
		}
		else {
			if (verbose) {
			dm <- t(pbapply(X, 2,
				function (x, ...) fidelity.method(
				x, cluster, method, group, alternative = alternative, ...)))
			}
			else {
				dm <- t(apply(X, 2,
					function (x, ...) fidelity.method(
						x, cluster, method, group, alternative = alternative, ...)))
			}
		}
	
		if (is.null(group)) {
			colnames(dm) <- levels(cluster)
		} else {
			dm <- t(dm)
			colnames(dm) <- group
		}
	
		#	warning! shoud not happen
		dm[is.na(dm)] <- 0
	
		#	Latex method needs rownames for indexing
		#	all.equal(rownames(dm), names(obj))
	
		#	IndVal.g returns the square root of indval (package(labdsv))!
		if (method == "IndVal.g") {
			dm <- dm ^ 2
		}
	})
	
	#	compute Fisher test
	
	#	get binary matrix if fidelity measure is based on abundances
	if (r.ind) X <- as.logical(obj)
	
	cpu.time.ft <- system.time({
	if (verbose) {
		pbapply::pboptions(char = ".")
	}	
	if (fast) {
		ft <- mclapply(as.data.frame(X),
			function (x, ...) fidelity.method(
			x, cluster, "Fisher", group, alternative = alternative))
		ft <- matrix(unlist(ft), ncol = nlevels(cluster), nrow = length(ft),
			dimnames = list(names(ft), levels(cluster)), byrow = TRUE)
	}
	else {
		ft <- t(pbapply::pbapply(X, 2,
			function (x, ...) fidelity.method(
			x, cluster, "Fisher", group, alternative = alternative)))
	}
	#}
	
	if (is.null(group)) {
		colnames(ft) <- levels(cluster)
	} else {
		ft <- t(ft)
		colnames(ft) <- group
	}
	})
	
	if (verbose) {
		message("computed fidelity measure ", method, " in ",
			round(cpu.time.dm[3], 3), " sec", appendLF = FALSE)
		message(" and Fisher test in ",
			round(cpu.time.ft[3], 3), " sec")
	}
	
	#	perform bootstrap of diagnostic values
	if (nboot > 0) {
		cat("perform bootstrap")
		if (verbose) {
			pb <- txtProgressBar(min = 0, max = nboot,
			char = '.', width = 45, style = 3)
		}		
		dmb <- array(0, dim = c(nboot, nsps, ngps))
		dmlower <- matrix(0, nsps, ngps)
		dmupper <- matrix(0, nsps, ngps)
		
		for (b in 1:nboot) {
			if (verbose) setTxtProgressBar(pb, b)
			bi <- sample(nsts, replace = TRUE)
			clusterb <- cluster[bi]
			for (i in 1:nsps) {
				savb <- X[bi, i]
				dmb[b, i, ] <- fidelity.method(savb, clusterb, method, group)
			}
		}
	
		for (i in 1:nsps) {	
			for (k in 1:ngps) {	
				sdmb <- sort(dmb[,i,k])
				dmlower[i, k] <- sdmb[(alpha / 2.0) * nboot]
				dmupper[i, k] <- sdmb[(1 - (alpha / 2.0)) * nboot]
			}
		}
		if (verbose) close (pb)
		dmlower <- data.frame(dmlower)
		dmupper <- data.frame(dmupper)
		row.names(dmlower) <- names(X)

		if (is.null(group))
			names(dmlower) <- levels(cluster)
		else
			names(dmlower) <- group
		
		row.names(dmupper) <- names(X)
		if (is.null(group)) {
			names(dmupper) <- levels(cluster)
		}
		else {
			names(dmupper) <- group
		}
	}
	else {
		dmlower = matrix(NA, nrow = nrow(dm), ncol = ncol(dm))
		dmupper = matrix(NA, nrow = nrow(dm), ncol = ncol(dm))
	}	# end nboot

	#	develop class VegsoupPartitionFidelity from class VegsoupPartition
	r <- new("VegsoupPartitionFidelity", obj)
	#	assign class slots
	r@stat <- as.matrix(dm)
	r@fisher.test <- as.matrix(ft)
	r@lowerCI <- as.matrix(dmlower)
	r@upperCI <- as.matrix(dmupper)
	r@nboot <- as.integer(nboot)
	r@fidelity.method <- method
return(invisible(r))

}

setGeneric("fidelity",
	function (obj, method = "r.g", group = NULL, nboot = 0, alpha = 0.05, c = 1, alternative = "greater", fast = FALSE, verbose = TRUE, ...)
	standardGeneric("fidelity")
)
setMethod("fidelity",
	signature(obj = "VegsoupPartition"),
	.fidelityVegsoupPartition
)

.SigFidelityVegsoupPartition <- function (obj, mode = 1, nperm = 999, alternative = "two.sided", verbose = TRUE) {

mode <- match.arg(as.character(mode), c("0","1"))
alternative <-  match.arg(as.character(alternative), c("greater","less","two.sided"))

X <- as.matrix(obj)

n.species <- ncol(X)
n.sites <- nrow(X)  
U <- partitioningMatrix(obj) 
k = getK(obj)

cdm = matrix(1, nrow = n.species, ncol = k)
ddm = matrix(1, nrow = n.species, ncol = k)
  
if (mode == 0) {
	dm = t(X) %*% U
} else {
	aisp <- t(X) %*% U
	ni <- diag(t(U) %*% U)
	aispni <- sweep(aisp, 2, ni, "/")
	aispni[is.na(aispni)] <- 0 # check for division by zero
	s <- apply(aispni, 1, "sum")
	dm <- sweep(aispni, 1, s, "/")
	dm[is.na(dm)] <- 0 # check for division by zero
}
	 	
if (verbose) {
	pb <- txtProgressBar(min = 0, max = nperm,
		char = '.', width = 45, style = 3)
}
cpu.time <- system.time({
	for (p in 1:nperm) {
		if (verbose) setTxtProgressBar(pb, p)
		pX <- as.matrix(X[sample(1:n.sites), ])
		if (mode == 0) {
			dmp <- t(pX)%*%U
		} else {
			aisp <- t(pX) %*% U
			ni <- diag(t(U) %*% U)
			aispni <- sweep(aisp,2, ni, "/")
			aispni[is.na(aispni)] <- 0 # check for division by zero
			s = apply(aispni, 1, "sum")
			dmp = sweep(aispni, 1, s, "/")
			dmp[is.na(dmp)] <- 0 # check for division by zero
		}
		if (alternative == "less") {
			cdm <- cdm + as.numeric(dmp <= dm)
		} else {
			if (alternative == "greater") {
				cdm <- cdm + as.numeric(dmp >= dm)
			} else {
				if (alternative == "two.sided") {
					cdm <- cdm + as.numeric(dmp >= dm) # greater
					ddm <- ddm + as.numeric(dmp <= dm) # less
				}
			}
		}
	}
})	# end system.time

if (verbose) close(pb)
	
if (alternative != "two.sided") {
	cdm <- cdm / (nperm + 1)
} else {
	cdm <- pmin(matrix(1, nrow = n.species, ncol = k),
	(2 * pmin(cdm,ddm)) / (nperm + 1))
}

rownames(cdm) <- colnames(X)
colnames(cdm) <- names(as.data.frame(U))

if (verbose) {
	cat("time to compute p-values based on",
	nperm, "permutations =", cpu.time[3], "sec",'\n')
}

p.Sidak = vector(mode = "numeric", length = n.species)
best = vector(mode = "numeric", length = n.species)
for (i in 1:n.species) {
	best[i] = which(cdm[i, ] == min(cdm[i, ]))[1]
	p.Sidak[i] = (1 - (1 - min(cdm[i, ])) ^ k)
}

res <- list(stat = cdm, sidak = data.frame(best, p.Sidak))
return(invisible(res))
}

setGeneric("SigFidelity",
	function (obj, mode, ...)
		standardGeneric("SigFidelity")
)
setMethod("SigFidelity",
	signature(obj = "VegsoupPartition"),
	.SigFidelityVegsoupPartition
)

#	get methods
setGeneric("getStat",
	function (obj, ...)
		standardGeneric("getStat")
)
setMethod("getStat",
	signature(obj = "VegsoupPartitionFidelity"),
	function (obj) obj@stat	
)	