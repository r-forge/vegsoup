.latexVegsoupDataPartitionSites <- function (object, p.col.width, filename, verbose = FALSE, ...) {
#	object  <- prt

sites <- object@sites

#	variables to drop for summary table	
drop <- grep("date", names(sites), fixed = TRUE)
drop <- c(drop, grep("longitude", names(sites), fixed = TRUE))
drop <- c(drop, grep("latitude", names(sites), fixed = TRUE))

if (missing(filename)) {
	filename <- "SitesPartitionTable"	
}
if (length(drop) > 0) {
	if (verbose) {
		cat("droped variables ",
		paste(names(sites)[drop], collapse = ", "),
		". not meaningful for summary")
	}	
	sites <- sites[ ,-drop]
}
if (missing(p.col.width)) {
	p.col.width = "15mm"
	if (verbose) {
		cat("p.col.width missing, set to ", p.col.width, call. = FALSE)
	}
}

part <- Partitioning(object)
num.cols <- sapply(sites, is.numeric)
char.cols <- sapply(sites, is.character)

num.cols.agg <- matrix(NA,
	ncol = length(which(num.cols)),
	nrow = getK(object))
	
for (i in seq(along = which(num.cols))) {
	i.median <- aggregate(sites[,which(num.cols)[i]], by = list(part), median)[,2]
	i.mad <- aggregate(sites[,which(num.cols)[i]], by = list(part), mad)[,2]
	num.cols.agg[,i] <- paste(i.median, " (", round(i.mad, 3), ")", sep = "")
}
num.cols.agg <- as.data.frame(num.cols.agg, stringsAsFactors = FALSE)
names(num.cols.agg) <- names(sites)[num.cols]

char.cols.agg <- matrix(NA,
	ncol = length(which(char.cols)),
	nrow = getK(object))
for (i in seq(along = which(char.cols))) {
	#	i = 1
	i.table <- data.frame(variable = sites[,which(char.cols)[i]], part)
	j.res <- c()
	for (j in 1:getK(object)) {
		j.tmp <- table(i.table[i.table$part == j,]$variable)
		j.tmp <- sort(j.tmp[j.tmp > 0], decreasing = TRUE)
		j.res <- c(j.res, paste(names(j.tmp), j.tmp, sep = ":", collapse = ", "))
	}
	
	char.cols.agg[,i] <- j.res
}

char.cols.agg <- as.data.frame(char.cols.agg, stringsAsFactors = FALSE)
names(char.cols.agg) <- names(sites)[char.cols]

tex <- res <- data.frame(partiton = 1:getK(object), num.cols.agg, char.cols.agg,
	stringsAsFactors = FALSE)
	
caption <- paste("Summary table for sites variables grouped in",
		getK(object),
		"partitions.",
		"Median and median absolute deviation in parentheses.",
		"Relevees per partition: ",
		paste(names(table(Partitioning(object))),
			table(Partitioning(object)), sep = ":", collapse = ", ")
		)
p.col <- paste("|p{", p.col.width, "}", sep = "")
col.just <- c(rep(p.col, ncol(tex)))
#col.just[ncol(num.cols.agg) + 1] <- paste("|", col.just[ncol(num.cols.agg) + 1], sep = "")
#	tex valid filenames
#	to do! see .latexVegsoupDataPartitionFidelity
#	more tests on filename
if (length(grep(".", "_", filename, fixed = TRUE))) {
		
}

if (length(grep(" ", filename, fixed = TRUE)) > 0) {
	warning("LaTex assumes no blanks in filenames!",
		" we replace all blanks!")
	filename <- gsub(" ", "_", filename, fixed = TRUE)	
}

if (length(grep(".tex", filename, fixed = TRUE)) < 1) {
	warning("add file extension .tex to filename ", filename)
	filename <- paste(filename, ".tex", sep = "")
}

latex(tex,
	file = filename,
	caption = caption,
	rowname = NULL,
	booktabs = TRUE,
	longtable = TRUE,
	lines.page = nrow(tex),
	here = TRUE,
	col.just = col.just,
	...)

return(invisible(res))
}

.latexVegsoupDataPartitionSpeciesRecursive <- function (object, p.col.width, path, ...) {
	
#	object  <- prt
if (missing(path)) {
	warning("no path supplied for LaTex files")
}	

if (missing(p.col.width)) {
	p.col.width = "10mm"
	warning("p.col.width missing, set to ", p.col.width, call. = FALSE)
}	

res <- vector("list", length = getK(object))
filenames <- c()
for(i in 1:getK(object)) {
	#	i = 1
	i.part <- object[Partitioning(object) == i, ]
	i.part <- Arrange(i.part)
	i.part <- i.part[,order(DecomposeNames(i.part)$layer)]
	
	res[[i]] <- i.part
	
	i.tex <- t(as.character(i.part))
	i.tex <- gsub("0", ".", i.tex, fixed = TRUE)
	i.tex <- cbind(DecomposeNames(i.part)[c("taxon", "layer")], i.tex)
	#	tex valid filenames
	filename <- paste(path, "species", i, ".tex", sep = "")
	filenames <- c(filenames, filename)
	caption <- paste("Cluster table", i)

	p.col <- paste("p{", p.col.width, "}", sep = "")
	col.just <- c("p{70mm}", "p{10mm}", rep(p.col, dim(i.part)[1]))
	col.names <- c("Taxon", "Layer", 1:getK(object))
	
	latex(i.tex,
	file = filename,
	caption = caption,
	rowname = NULL,
	booktabs = TRUE,
	longtable = TRUE,
	lines.page = nrow(i.tex),
	here = TRUE,
	col.just = col.just) 
}


con <- file(paste(path, "species.tex", sep = ""))
	writeLines(paste("\\input{../", filenames, "}", sep = ""), con)
close(con)

return(invisible(res))
}

.latexVegsoupDataPartitionSitesRecursive <- function (object, path, ...) {
	#	to do!	
}

setGeneric("Latex",
	function (object, ...)
		standardGeneric("Latex")
)

setMethod("Latex",
	signature(object = "VegsoupDataPartition"),
	function (object, choice, recursive, ...) {
			if (missing(choice)) {
				choice  <- "species"	
			}
			if (missing(recursive)) {
				recursive  <- FALSE
			}			
			if (choice == "sites" & !recursive) {
				.latexVegsoupDataPartitionSites(object, ...)
			}
			if (choice == "species" & !recursive) {
				.latexVegsoupDataPartitionFidelity(Fidelity(object, ...))
			}
			if (choice == "sites" & recursive) {
				.latexVegsoupDataPartitionSitesRecursive(object, ...)
			}
			if (choice == "species" & recursive) {
				.latexVegsoupDataPartitionSpeciesRecursive(object, ...)
			}			
	}
)

#	returns a list occurences of species in partitions
.spreadVegsoupDataPartiton <- function (object) {
#		
if (!inherits(object, "VegsoupDataPartition"))
	stop("need object of class VegsoupDataPartition")

#	debug
#	object = prt
part  <- Partitioning(object)
X <- as.binary(object)

res <- apply(X, 2, function (y) {
		if (getK(object) > 1) {
			sapply(rownames(X[y > 0,]),
				function (z) part[which(names(part) == z)],
					USE.NAMES = FALSE)
		} else {
			warning("single partition not meaningful")
		}
	}
)
return(res)
}

setGeneric("Spread",
	function (object)
		standardGeneric("Spread")
)
setMethod("Spread",
    signature(obj = "VegsoupDataPartition"),
	.spreadVegsoupDataPartiton
)