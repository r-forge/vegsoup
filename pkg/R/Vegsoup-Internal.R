#	internal function to melt sites data frame
.melt <- function (obj) {
	#	Suggests:
	require(stringr)
	#	obj = allargs[[10]]
	res <- data.frame(plot = rownames(slot(obj, "sites")),
				as.matrix(slot(obj, "sites")), stringsAsFactors = FALSE)
	res <- reshape(res, direction = "long",
		varying = names(res)[-1],
		v.names = "value",
		timevar = "variable",
		times = names(res)[-1],
		idvar = "plot", new.row.names = NULL)
	#	order by plot and create sequential rownames!	
	res <- res[order(res$plot), ]
	rownames(res) <- 1:nrow(res)
	#	width of numbers
	res$value <- str_trim(res$value, side = "left")
	res	
}

#	helper function for VegsoupPartition
#	credits go to Dave Roberts
.VegsoupPartitionOptpartBestopt <- function (dist, k, numitr, verbose = TRUE) {
    if (class(dist) != "dist") {
        stop("bestopt is only defined for objects of class dist")
    }
    #	Imports: optpart
    #	require(optpart)
    
    best <- 0
    ratios <- rep(0, numitr)
    for (i in 1:numitr) {
    	if (verbose) cat(".")
		tmp <- optpart::optpart(k, dist)        
        while (max(tmp$clustering) != k) {
        	tmp <- optpart::optpart(k, dist)
        	}
        ratios[i] <- max(tmp$ratio)
        if (ratios[i] > best) {
            best <- ratios[i]
            result <- tmp
            itr <- i
        }
    }
    cat("\nRatios for respective partitions \n")
    print(format(ratios, digits = 4))
    cat(paste("\nChoosing # ", itr, " ratio = ", format(best, 
        digits = 4), "\n"))
    invisible(result)
}


.texfile <- function (x, verbose, ...) {
	#	add file extension if missing
	if (length(grep(".tex", x, fixed = TRUE)) < 1) {
		x = paste0(x, ".tex")
	}
	#	replaced all blanks with underscores
	if (length(grep(" ", x, fixed = TRUE)) > 0) {
		x = gsub(" ", "_", x, fixed = TRUE)	
	}
	return(x)
}

#	a little bit of old materials

#N <- nrow(obj)						# number of plots
#n.i <- colSums(getBin(obj))			# species frequencies
#N_pi <- table(Partitioning(obj))	# number of plots in partition
#n_pi <- Contingency(obj)			# number of occurences in partition

#	notation follows Bruelheide (1995, 2000)
#	cited in Chytry et al 2002:80

#	N: number of plots in the data set
#	N_p: number of plots in partition
#	n: number of occurences in the data set
#	n_p: number of occurences in parttion 

#ObservedFreqencyTable <- function (N, N_p, n, n_p) { # f(o)_i
#	res <- matrix(c(
#		n_p,			# n_pi[i,j] for the the i species
#		N_p - n_p,
#		n - n_p,
#		N - N_p - n + n_p), 2, 2)	
#	res[is.nan(res)] <- 0			# f(o)_i
#   return(res)	
#}

#ExpectedFreqencyTable <- function (N, N_p, n, n_p) { # f(e)_i
#	res <- matrix(c(
#		n * N_p / N,
#		(N - n) * N_p / N,
#		n * (N - N_p) / N,
#		(N -n) * (N - N_p) / N), 2, 2)	
#	res[is.nan(res)] <- 0
#   return(res)
#}

#for (i in 1:ncol(obj)) { # loop over species
#	n <- n.i[i]						# n	
#   for (j in 1:length(N_pi)) {		# loop over partitions
#		N_p <- N_pi[j]				# N_p
#		n_p <- n_pi[i,j]    	
#    foi <- ObservedFreqencyTable(N, N_p, n, n_p)
#    fei <- ExpectedFreqencyTable(N, N_p, n, n_p)
#    res1 <- 2 * sum(foi * log(foi / fei), na.rm = T)
#    res2 <- g.statistic(fei, correction = "none")
#    }
#}	


#".sidak" <- function(vecP) {
#
# This function corrects a vector of probabilities for multiple testing
# using the Bonferroni (1935) and Sidak (1967) corrections.
#
# References: Bonferroni (1935), Sidak (1967), Wright (1992).
#
# Bonferroni, C. E. 1935. Il calcolo delle assicurazioni su gruppi di teste. 
# Pp. 13-60 in: Studi in onore del Professore Salvatore Ortu Carboni. Roma.
#
# Sidak, Z. 1967. Rectangular confidence regions for the means of multivariate 
# normal distributions. Journal of the American Statistical Association 62:626-633.
#
# Wright, S. P. 1992. Adjusted P-values for simultaneous inference. 
# Biometrics 48: 1005-1013. 
#
#                  Pierre Legendre, May 2007
#	k = length(vecP)
#	
#	vecPB = 0
#	vecPS = 0
#	
#	for(i in 1:k) {
#	   bonf = vecP[i]*k
#	   if(bonf > 1) bonf=1
#	   vecPB = c(vecPB, bonf)
#	   vecPS = c(vecPS, (1-(1-vecP[i])^k))
#	   }
#	#
#	return(list(OriginalP=vecP, BonfP=vecPB[-1], SidakP=vecPS[-1]))
#}
