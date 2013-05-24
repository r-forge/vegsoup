SpeciesTaxonomy <- function (x, y, file.x, file.y, csv2 = TRUE, pmatch = FALSE, skip = TRUE, verbose = FALSE) {

#	x = X
#	y = txa
#	tmp <- c(x = F, y = T, file.x = T, file.y = F)

#	test inputs
test <- combn(c("x", "y", "file.x", "file.y"), 2)
cmb <- test <- test[, c(1, 3, 4, 6)]

mis <- c(x = !missing(x), y = !missing(y),
	file.x = !missing(file.x), file.y = !missing(file.y))

for (i in seq(along = mis)) {
	test[test == names(mis[i])] <- mis[i]
}

mode(test) <- "logical"

sel <- apply(test, 2, all)
if (all(sel == FALSE)) {
	stop("please supply x respectively file.x and y respectively file.y")
}	
if (sum(as.numeric(sel)) > 1) {
	cat("supplied", paste(cmb[, sel], collapse = " and "), "\n")
	stop("\ni don't know what to choose?")
}

if (which(sel) == 1) {
	if (inherits(x, "Species")) {
		X <- species(x)
	} else {
		X <- species(new("Species", data = x)) # ensures validity
	}
	if (inherits(y, "Taxonomy")) {	
		Y <- taxonomy(y)
	} else {
		Y <- taxonomy(new("Taxonomy", data = y)) # ensures validity
	}		
}

if (which(sel) == 2) {
	#message("here")
	if (inherits(x, "Species")) {
		X <- species(x)
	} else {
		X <- species(new("Species", data = x))
	}	
	y <- read.csv2(file.y,
		sep = ifelse(csv2, ";", ","), dec = ifelse(csv2, ",", "."),
		stringsAsFactors = FALSE, check.names = FALSE)	
	Y <- taxonomy(new("Taxonomy", data = y))
}

if (which(sel) == 3) {
	x <- read.csv2(file.x,
		sep = ifelse(csv2, ";", ","), dec = ifelse(csv2, ",", "."),
		stringsAsFactors = FALSE, check.names = FALSE)
	X <- species(new("Species", data = x))	
	if (inherits(y, "Taxonomy")) {	
		Y <- taxonomy(y)
	} else {
		Y <- taxonomy(new("Taxonomy", data = y))
	}		
}

if (which(sel) == 4) {
	x <- read.csv2(file.x,
		sep = ifelse(csv2, ";", ","), dec = ifelse(csv2, ",", "."),
		stringsAsFactors = FALSE, check.names = FALSE)	
	y <- read.csv2(file.y,
		sep = ifelse(csv2, ";", ","), dec = ifelse(csv2, ",", "."),
		stringsAsFactors = FALSE, check.names = FALSE)
	X <- species(new("Species", data = x))
	Y <- taxonomy(new("Taxonomy", data = y))
}

#	check unique abbrevations
if (length(unique(Y$abbr)) != nrow(Y)) {
	stop("abbr has to be unique")
} else {
	rownames(Y) <- Y$abbr	
}
	
test1 <- match(unique(X$abbr), Y$abbr)

if (any(is.na(test1))) {
	test1 <- unique(X$abbr)[is.na(test1)]
	cat(paste("the following abbrevation(s) used in",
	cmb[1,sel],
	"were not found in supplied reference list",
	cmb[2,sel],
	"\n"))
	print(test1)
	cat("did you mean?\n")
	test1.pmatch <- matrix(c(test1, Y$abbr[pmatch(test1, Y$abbr)]), ncol = 2)
	print(test1.pmatch)
	if (pmatch) {
		for (i in 1:nrow(test1.pmatch)) {
			X$abbr[X$abbr == test1.pmatch[i,1]] <- test1.pmatch[i,2]
		}
		cat("replaced", test1.pmatch[,1])	
	} else {
		cat("if that is correct you can force me to replace those abbreviations!")
		cat("\ncall the function again with option pmatch = TRUE")
	}
}

test2 <- dim(X)[1] - dim(unique(X[,c(1:4)]))[1]

if (test2 > 0) {
	warning("\nspecies data not unique for ", test2, " sample(s)")
	if (verbose) {
		cat("\n")
		print(X[duplicated(X[ ,c(1:4)]), ])
	}
	X <- X[!duplicated(X[, c(1:4)]), ]
	warning("\nremoved duplicted species:\n\n")
} else {
	if (verbose) {
		cat("\nno duplicates found")
	}
}

Y <- Y[as.character(unique(X$abbr)), ]

if (any(is.na(Y[, 1]))) {
	test3 <- as.character(unique(X$abbr))[is.na(Y[, 1])]
	cat("\nnot found the following abbrevation(s) in supplied reference list\n")
	print(test3)
	#	to do!
	#	implement pmatch as above
	return(test1.pmatch)
	stop("Please review your reference list, you might need to include some new taxa")
}

if (any(is.na(X[, 1:4]))) {
	warning("\n... NAs introduced")
	cat(apply(X, 2, function (x) any(is.na(x))) )
}

res <- new("SpeciesTaxonomy",
	species = new("Species", data = X),
	taxonomy = new("Taxonomy", data = Y)) # [, 1:2]

#if (skip) {
#	species(res) <- species(res)[, 1:4]
#	taxonomy(res) <- taxonomy(res)[, 1:2]
#}
#return(new("Species", data = X))
return(invisible(res))
}
