#	package coenoflex is missing from CRAN since Summer 2014
#	switched to package coenocliner

coenoflex <- function (n = 30, m = 20, gradients = 2, ...) {
	#	taken from coenocliner vignette page 9
	N <- n # number of plots
	M <- m # number of species
	
	# first gradient
	min1 <- 0
	max1 <- 1
	loc1 <- seq(min1, max1, length = N)[sample(1:N, N)] # shuffle
	opt1 <- runif(M, min = min1, max = max1)
	tol1 <- rep(0.25, M)
	h <- ceiling(rlnorm(M, meanlog = 3)) # mimic percentages
	h <- ceiling(h / max(h) * 100)
	par1 <- cbind(opt = opt1, tol = tol1, h = h)
	
	# second gradient
	min2 <- 0
	max2 <- 1
	loc2 <- seq(min2, max2, length = N)[sample(1:N, N)]
	opt2 <- runif(M, min = min2, max = max2)
	tol2 <- ceiling(runif(M, min = 0.15, max = 0.85))
	par2 <- cbind(opt = opt2, tol = tol2)
		
	if (gradients == 1) {
		locs <- loc1
		pars <- par1
	} 
	
	if (gradients == 2) {		
		pars <- list(px = par1, py = par2)
		locs <- cbind(x = loc1, y = loc2)	
	} 

	r <- coenocline(locs, responseModel = "gaussian",
			params = pars,
			expectation = FALSE)
	r <- as(r, "Vegsoup")
			
	return(r)		
}	