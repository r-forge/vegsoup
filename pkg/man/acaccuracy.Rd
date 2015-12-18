#	calculate prediction accuracy statistics for two partitionings
#if(!isGeneric("accuracy")) {
setGeneric("accuracy",
	function (obj1, obj2)
		standardGeneric("accuracy")
)
#}

setMethod("accuracy",
	signature(obj1 = "VegsoupPartition",
		obj2 = "VegsoupPartition"),
	function (obj1, obj2) {

	if (getK(obj1) != getK(obj2)) {
		stop("Numbers of k differ for obj1 (", getK(obj1), ") ",
			"and obj2 (", getK(obj1), ")!", sep = "")
	}
	#	this is a slightly modified copy of function crosstable.statistics in package polytomous

	#	original author
	#	(C) Antti Arppe 2007-2011
	#	E-mail: antti.arppe@helsinki.fi

	#	Menard, S. (1995). Applied Logistic Regression Analysis.
	#	Sage University Paper Series on Quantitative Applications in the Social Sciences 07-106.
	#	Thousand Oaks: Sage Publications.
	
	#	reference (observed) as row margins, comparison (predicted) as column margins
	# 	according to Menard (1995: 24-32)
	
	N <- length(partitioning(obj1))
	
	#	contingency table
	X <- table(partitioning(obj1), partitioning(obj2))
	
	sum_r <- apply(X, 1, sum) # sum of rows
	sum_c <- apply(X, 2, sum) # sum of columns
	cor_m <- sum(diag(X)) # correct with model
	err_m <- N - cor_m # errors with model
	
	err_p <- N - max(sum_r) # errors without model prediction
	err_c <- sum(sum_r * ((N - sum_r) / N))
	lambda.p <- 1 - (err_m / err_p)
	
	d.lambda.p <- (err_p / N - err_m / N) / sqrt((err_p / N) * (1 - err_p / N) / N)
	p.lambda.p <- 1 - pnorm(d.lambda.p)
	tau.p <-	1 - (err_m/err_c)
	d.tau.p <- (err_c / N-err_m / N) / sqrt((err_c/N) * (1-err_c / N) / N)
	p.tau.p <- 1 - pnorm(d.tau.p)
	accuracy <- sum(diag(X)) / N
	recall.predicted <- diag(X) / sum_r
	precision.predicted <- diag(X) / sum_c

	res <- list(
	accuracy = accuracy,
	recall.predicted = recall.predicted,
	precision.predicted = precision.predicted,
	lambda.prediction = lambda.p,
	tau.classification = tau.p,
	d.lambda.prediction = d.lambda.p,
	d.tau.classification = d.tau.p,
	p.lambda.prediction = p.lambda.p,
	p.tau.classification = p.tau.p)
	
	return(res)
}
)
