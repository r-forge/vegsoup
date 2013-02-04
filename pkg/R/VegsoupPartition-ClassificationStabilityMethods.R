#	adopted for vegsoup from script Function Classification Stability supplied for JUICE
#	nitr: number of iterations
#	...: arguments passed to VegsoupPartition
StablePartition <- function (obj, nitr = 200, nitr.lambda = 10, ...)	{
#obj <- prt
#nitr = 20

k = getK(obj)

pb <- txtProgressBar (min = 1, max = nitr, char = '.',
	width = 45, style = 3, title = 'Calculation progress')

#setTxtProgressBar (pb, 1)
   
prt <- VegsoupPartition(obj, k = k)

rnd.lambda <- 0
lambda <- vector(mode = 'numeric', length = nitr)
		
for (i in 1:nitr) {
	#	i = 1
	if (round(i/10, 0) == i/10) {
		setTxtProgressBar (pb, i)
	}
#	subset selection without replacement?
#	replace implies the opposite?

	sub <- suppressWarnings(SampleVegsoup(obj, replace = TRUE))
		
#	Subset classification
	prt.sub <- VegsoupPartition(sub, k = k, ...)
	prt.tmp <- prt[rownames(prt) %in% rownames(prt.sub),]
		
#	Raw lambda calculation
	lambda[i] <- Confus(prt.tmp, prt.sub)$lambda
	
#	Random lambda calculation (10 times)

	for (j in 1:nitr.lambda) {
   		a <- factor(sample(1:k, dim(prt.sub)[1], replace = TRUE), levels = 1:k)
   		b <- factor(sample(1:k, dim(prt.sub)[1], replace = TRUE), levels = 1:k)
   		m <- table(a, b)
   		q1 <- sum(apply(m, 1, function(x) max(x)))
   		q2 <- sum(apply(m, 2, function(x) max(x)))
   		q3 <- max(rowSums(m))
   		q4 <- max(colSums(m))
   		q5 <- 2 * sum(m)
   		lambda.j <- (q1 + q2 - q3 - q4) / (q5 - q3 - q4)
   		rnd.lambda <- rnd.lambda + lambda.j
	}
}

#	Final modified lambda calculation

lambda <- sum(lambda) / nitr
rnd.lambda <- rnd.lambda / (nitr * nitr.lambda)
mod.lambda <- (lambda - rnd.lambda) / (1 - rnd.lambda)

res <- list(lambda = lambda, modified.lambda = mod.lambda)
res
}