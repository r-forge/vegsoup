#	... arguments passed to VegsoupDataPartition

OptimClass <- function (obj, k, ft.treshold = 1e-3, oc.treshold = 2, alternative = "two.sided", method = c("ward", "flexible", "pam", "kmeans", "wards"), ...) {
	#	obj = dta
	cycle <- function (obj, k, ...) {
		prt <- VegsoupDataPartition(obj, k = k, ...)
		ft <- FisherTest(prt, alternative = alternative)
		ind <- apply(ft < ft.treshold, 2, sum)
		OptimClass1 <- sum(ind)
		OptimClass2 <- sum(ind >= oc.treshold)
		res <- c(OptimClass1, OptimClass2)
		return(res)
	}

	res.i <- vector("list", length = length(method))
	names(res.i) <- method

#	pb.i <- txtProgressBar(min = 1, max = length(res.i),
#		char = '.', width = 45, style = 3)
		
	for (i in seq(along = method)) {
#		setTxtProgressBar(pb.i, i)
		cat(method[i])
		res.j <- matrix(NA, nrow = k - 1, ncol = 2)
		pb.j <- txtProgressBar(min = 2, max = k,
			char = '.', width = 45, style = 3)		
		for (j in 2:k) {
			setTxtProgressBar(pb.j, j)
			res.j[j - 1, ] <- cycle(obj, method = method[i], k = j, ...)#
		}
		res.j <- rbind(c(0, 0), res.j)
		dimnames(res.j) <- list(1:k, c("OptimClass1", "OptimClass2"))
		res.i[[i]] <- res.j
		close(pb.j)
	}	
		
	settings  <- list(k = k, ft.treshold = ft.treshold, oc.treshold = oc.treshold,
		method = method)
#	close (pb.i)	
	return(list(OptimClass = res.i, settings = settings))
}

plotOptimClass <- function (x, mode = c(1, 2)) {
	type <-  match.arg(as.character(mode), c("1", "2"))
	#	x = oc
	if (missing(mode)) mode = "1"
	k <- x$settings$k
	ft.treshold <- x$settings$ft.treshold
	oc.treshold <- x$settings$oc.treshold
	ylim.1 <- max(sapply(x$OptimClass, function(x) max(x[, 1])))
	ylim.2 <- max(sapply(x$OptimClass, function(x) max(x[, 2])))
	
	switch(mode,
		"1" = {
		plot(1:k, rep(max.oc, k),
			type = "n", ylim = c(0, ylim.1),
			xlab = "OptimClass1",
			ylab = "No. of faithfull species",
			sub = paste("Fisher's exact test, treshold",
			format(ft.treshold,scientific = TRUE)))
		for (i in seq(along = names(x$OptimClass))) {
			xx = 1:k
			yy = x$OptimClass[[i]][, 1]
			lines(xx, yy, lty = i)
		}
	}, "2" = {
		plot(1:k, rep(max.oc, k),
			type = "n", ylim = c(0, ylim.2),
			xlab = "OptimClass1",
			ylab = paste("No. of cluster with more than", oc.treshold, "faithful species"),
			sub = paste("Fisher's exact test, treshold",
			format(ft.treshold,scientific = TRUE)))
		for (i in seq(along = names(x$OptimClass))) {
			xx = 1:k
			yy = x$OptimClass[[i]][, 2]
			lines(xx, yy, lty = i)
		}	
	})
	legend("topright", lty = 1:length(method), legend = names(oc$OptimClass))
}

