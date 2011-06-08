cluster <- Partitioning(prt)
clnames <- levels(as.factor(cluster))

# Matrix of possible cluster combinations
cl.comb <- function (clnames) {
	  k <- length(clnames) # k <- getK(prt)
    ep <- diag(1,k,k)
    names.ep <- clnames
    for(j in 2:k) {
    	cat(j)
      nco <- choose(k,j)
      co <- combn(k,j)
      epn <- matrix(0,ncol=nco,nrow=k)
      for(i in 1:ncol(co)) {
      	cat(i)
	  epn[co[,i],i] <- 1
	  names.ep <- c(names.ep, paste(clnames[co[,i]], collapse = "+"))
      }
      ep <- cbind(ep,epn)
    }
    colnames(ep) <- names.ep
    return(ep)
}

foo <- cl.comb(cluster)