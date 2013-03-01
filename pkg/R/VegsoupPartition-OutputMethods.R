setMethod("write.verbatim",
   signature(obj = "VegsoupPartition"),
		.write.verbatimVegsoup
)

#	to do: add column for indicator value, high priority!

.latexVegsoupPartitionSites <- function (obj, col.width, filename, verbose = FALSE, ...) {
#	obj  <- prt

sites <- obj@sites

#	variables to drop for summary table	
drop <- grep("date", names(sites), fixed = TRUE)
drop <- c(drop, grep("longitude", names(sites), fixed = TRUE))
drop <- c(drop, grep("latitude", names(sites), fixed = TRUE))

if (missing(filename)) {
	filename <- "SitesPartitionTable"	
}
if (length(drop) > 0) {
	if (verbose) {
		cat("dropped variables ",
		paste(names(sites)[drop], collapse = ", "),
		". not meaningful for summary")
	}	
	sites <- sites[ ,-drop]
}
if (missing(col.width)) {
	col.width <- "15mm"
	if (verbose) {
		cat("col.width missing, set to ", col.width)
	}
}

part <- Partitioning(obj)

num.cols <- sapply(sites, is.numeric)
char.cols <- sapply(sites, is.character)

num.cols.agg <- matrix(NA,
	ncol = length(which(num.cols)),
	nrow = getK(obj))
	
for (i in seq(along = which(num.cols))) {
	i.median <- aggregate(sites[, which(num.cols)[i]], by = list(part), median)[ ,2]
	i.mad <- aggregate(sites[, which(num.cols)[i]], by = list(part), mad)[ ,2]
	num.cols.agg[,i] <- paste(i.median, " (", round(i.mad, 3), ")", sep = "")
}

num.cols.agg <- as.data.frame(num.cols.agg, stringsAsFactors = FALSE)
names(num.cols.agg) <- names(sites)[num.cols]

char.cols.agg <- matrix(NA,
	ncol = length(which(char.cols)),
	nrow = getK(obj))
	
for (i in seq(along = which(char.cols))) {
	#	i = 1
	i.table <- data.frame(variable = sites[,which(char.cols)[i]], part)
	j.res <- c()
	for (j in 1:getK(obj)) {
		j.tmp <- table(i.table[i.table$part == j,]$variable)
		j.tmp <- sort(j.tmp[j.tmp > 0], decreasing = TRUE)
		j.res <- c(j.res, paste(names(j.tmp), j.tmp, sep = ":", collapse = ", "))
	}	
	char.cols.agg[,i] <- j.res
}

char.cols.agg <- as.data.frame(char.cols.agg, stringsAsFactors = FALSE)
names(char.cols.agg) <- names(sites)[char.cols]

#	add plots to partition column
part.plot <- data.frame(part, names(part))
names(part.plot) <- c("partition", "plots")
part.plot <- part.plot[order(part.plot$partition),]

part.plot <- sapply(unique(part.plot$partition),
	function (x) {
		paste(x, paste(part.plot[part.plot$partition == x, 2], collapse = ", "), sep = ": ")
	}
)

tex <- res <- data.frame(
	partiton = part.plot,
	num.cols.agg, char.cols.agg,
	stringsAsFactors = FALSE)
	
caption <- paste("Summary table for sites variables grouped in",
		getK(obj),
		"partitions.",
		"Median and median absolute deviation in parentheses.",
		"Relevees per partition: ",
		paste(names(table(Partitioning(obj))),
			table(Partitioning(obj)), sep = ":", collapse = ", ")
		)
p.col <- paste("|p{", col.width, "}", sep = "")
col.just <- c(rep(p.col, ncol(tex)))
#col.just[ncol(num.cols.agg) + 1] <- paste("|", col.just[ncol(num.cols.agg) + 1], sep = "")
#	tex valid filenames
#	to do! see .latexVegsoupPartitionFidelity
#	more tests on filename
if (length(grep(".", "_", filename, fixed = TRUE))) {
		
}

if (length(grep(" ", filename, fixed = TRUE)) > 0) {
	warning("LaTex assumes no blanks in filenames!",
		" we replace all blanks!")
	filename <- gsub(" ", "_", filename, fixed = TRUE)	
}

if (length(grep(".tex", filename, fixed = TRUE)) < 1) {
	if (verbose) {
		cat("add file extension .tex to filename ", filename)
	}
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

#	\dots passed to seriation()
.latexVegsoupPartitionSpeciesRecursive <- function (obj, path, col.width, taxa.width, caption.text, verbose, ...) {
	
#	obj  <- prt
if (missing(path)) {
	warning("no path supplied for LaTex files")
}	
if (missing(verbose)) {
	verbose = FALSE
}
if (missing(col.width)) {
	col.width <- "10mm"
	if (verbose) {
		cat("col.width missing, set to ", col.width, call. = FALSE)
	}	
}	
if (missing(taxa.width)) {
	taxa.width <- "60mm"
	if (verbose) {
		cat("col.width missing, set to ", taxa.width, call. = FALSE)
	}	
}
if (missing(caption.text)) {
	caption.text <- ""
	if (verbose) {
		cat("caption.text missing, set to", caption.text, call. = FALSE)
	}	
}

res <- vector("list", length = getK(obj))
filenames <- c()

for (i in 1:getK(obj)) {
	#	obj = prt; i = 2
	i.part <- obj[Partitioning(obj) == i, ]
	i.part <- seriation(i.part, ...)
	#	table will be order according to Layers(obj)
	#	was i.part <- i.part[, order(DecomposeNames(i.part)$layer, decreasing = TRUE)]
	
	res[[i]] <- i.part
	
	i.tex <- t(as.character(i.part))
	i.tex <- gsub("0", ".", i.tex, fixed = TRUE)

	i.tex <- cbind(DecomposeNames(i.part)[c("taxon", "layer")], i.tex)
	#	tex valid filenames
	filename <- paste(path, "species", i, ".tex", sep = "")
	filenames <- c(filenames, filename)
	caption <- paste("Sample table of Cluster", i)

	p.col <- paste("p{", col.width, "}", sep = "")
	col.just <- c(paste("p{", taxa.width, "}", sep = ""), "p{10mm}",
		rep(p.col, dim(i.part)[1]))
	
	col.heads = c("Taxon", "Layer", paste("\\rotatebox{90}{", dimnames(i.tex)[[2]][-c(1,2)], "}"))
	
	latex(i.tex,
	file = filename,
	caption = paste(caption, caption.text, collapse = " "),
	rowname = NULL,
	booktabs = TRUE,
	longtable = TRUE,
	lines.page = nrow(i.tex),
	here = TRUE,
	col.just = col.just,
	colheads = col.heads) 
}


con <- file(paste(path, "species.tex", sep = ""))
	writeLines(paste("\\input{",
			gsub(path, "", filenames, fixed = TRUE),
			"}", sep = ""), con)
close(con)

return(invisible(res))
}

.latexVegsoupPartitionSitesRecursive <- function (obj, path, ...) {
	#	to do!	
}

setGeneric("Latex",
	function (obj, ...)
		standardGeneric("Latex")
)

setMethod("Latex",
	signature(obj = "VegsoupPartition"),
	function (obj, choice, recursive, ...) {
			require(Hmisc)
			if (missing(choice)) {
				choice <- "species"	
			}
			if (missing(recursive)) {
				recursive <- FALSE
			}			
			if (choice == "sites" & !recursive) {
				res <- .latexVegsoupPartitionSites(obj, ...)
			}
			if (choice == "species" & !recursive) {
				res <- .latexVegsoupPartitionSpecies(obj, ...)
			}
			if (choice == "sites" & recursive) {
				res <- .latexVegsoupPartitionSitesRecursive(obj, ...)
			}
			if (choice == "species" & recursive) {
				res <- .latexVegsoupPartitionSpeciesRecursive(obj, ...)
			}			
	return(invisible(res))
	}
)