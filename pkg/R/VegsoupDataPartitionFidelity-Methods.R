.summaryVegsoupDataPartitionFidelity  <- function (object, ...) {
	cat("method", object@method)
	if (all(is.na(object@lowerCI))) {
		cat("\nno bootstrap performed")	
	} else {
		cat("\nnumber of bootstrap replicates", object@nboot)
	}
}

setMethod("summary",
    signature(object = "VegsoupDataPartitionFidelity"),
	.summaryVegsoupDataPartitionFidelity
)

setGeneric("getStat",
	function (obj, ...)
		standardGeneric("getStat")
)
setMethod("getStat",
	signature(obj = "VegsoupDataPartitionFidelity"),
	function (obj) obj@stat	
)

#	format and arrange fidelity table
#	adapted and extend from Sebastian Schmidtlein's isotab()
.latexVegsoupDataPartitionFidelity <- function (object, p.col.width, p.max, filename, stat.min, footer.treshold, molticols.footer, verbose = FALSE, letters = FALSE, ...) {
#	object = fid.prt
if (missing(filename)) {
	filename <- paste("FidelityTable")
}
if (missing(p.col.width)) {
	p.col.width <- "10mm"
	warning("p.col.width missing, set to ", p.col.width, call. = FALSE)
}
if (missing(p.max)) {
	p.max <- .05
	warning("p.max missing, set to ", p.max, call. = FALSE)
}
if (missing(footer.treshold)) {
	footer.treshold <- 2
}
if (missing(molticols.footer)) {
	molticols.footer <- 3
}
cntn <- Contingency(object)
cnst <- Constancy(object)
nc <- ncol(cnst)
sp <- ncol(object)

#	can do, can also be a method for class VegsoupDataPartition
#	if fidelity measure is calculöated by a some defaults?

ft <- object@fisher.test
N <- nrow(object)
frq <- colSums(as.binary(object))
siz <- table(Partitioning(object))

if (missing(stat.min) & object@method == "r.g") {
	#	automatic guess adapted from isopam()
	stat.min <- round (0.483709 + nc * -0.003272 + N * -0.000489 + sp * 0.000384 + sqrt (nc) * -0.01475, 2) 
} else {
	if (missing(stat.min)) {
		stat.min = 0
	}
}

#	significance symbols
symb <- ft
symb[ft > 0.05] <- ""
symb[ft <= 0.05] <- "*"
symb[ft <= 0.01] <- "**"
symb[ft <= 0.001] <- "***"

#	combine frequency table with significance symbols
frq.ft <- matrix(paste(cnst, symb, sep = ""), 
	nrow = nrow(cnst), ncol = ncol(cnst))
frq.ft <- data.frame(frq.ft)
colnames(frq.ft) <- 1:getK(object)
rownames(frq.ft) <- names(object)
  
#	fidelity measure
stat <- object@stat

#	sort table
stat.idx <- apply(stat, 1, which.max)	# group association by fidelity measure
frq.ord <- stat.idx

for (i in 1:length(frq.ord)) {
	frq.ord[i] <- cnst[i, stat.idx [i]]
}	

#	sorting
frq.top <- as.matrix(frq)[order(stat.idx, -frq.ord), ]
ord.top <- names(frq.top)
frq.ft.top <- frq.ft[ord.top, ]
ft <- ft[ord.top, ]
stat <- stat[ord.top, ]

#	filter diagnostic species
filter1 <- apply(ft, 1, min) <= p.max
filter2 <- apply(stat, 1, max) >= stat.min
#	diagnostic species
dia <- which(filter1 [filter2 == TRUE] == TRUE)

if (length(dia) == 0) diag <- "No diagnostic species with given thresholds." 
if (length(dia) > 0) diag <- frq.ft.top[names(dia), ]

#	for later use in the bottom part of the tables
ord.bot <- names(as.matrix(frq)[order(-frq), ])
frq.ft.b <- frq.ft[ord.bot, ]
    
#	move diagnostic species to top
if (length(dia) > 0) {
	res <- rbind(diag,
		frq.ft.b[rownames(frq.ft.b) %in% rownames(diag) == FALSE, ])
} else {
	res <- frq.ft.b
}

#	info about diagnostic species
dig1 <- stat.idx[names(stat.idx) %in% names (dia)]
dig2 <- dig1[rownames(diag)]
typ <- list ()
for (i in 1:nc) {
	if (length(names(dig2)[dig2 == i]) > 0) {
		typ [[i]] <- c(names(dig2)[dig2 == i])
	} else {
		typ [[i]] <- "Nothing particularly typical"
	}	
}
names(typ) <- colnames(cnst)

res <- list(tab = res, typical = typ)
#	typical = sapply(typ, function (x) paste(x, collapse = ', '))

#	sort layers
if (length(dia) > 0) {
	#	top of table, diagnostic/typical species
	txn <- DecomposeNames(object, verbose = FALSE)
	txn <- txn[match(rownames(res$tab), txn$abbr.layer), ]
	rownames(txn) <- txn$abbr.layer
	txn.top <- txn[rownames(diag), ]
	txn.top <- txn.top[order(txn.top$layer),]
	top <- res$tab[rownames(txn.top), ]
	
	#	bottom of table, remaining species
	txn <- DecomposeNames(object, verbose = FALSE)
	txn <- txn[match(rownames(res$tab), txn$abbr.layer), ]
	rownames(txn) <- txn$abbr.layer
	txn.bottom <- txn[-match(rownames(diag), rownames(res$tab)), ]
	txn.bottom <- txn.bottom[order(txn.bottom$layer),]
	bottom <- res$tab[rownames(txn.bottom), ]
	res$tab <- rbind(top, bottom)
} else {
	txn <- DecomposeNames(object, verbose = FALSE)
	txn <- txn[match(rownames(res$tab), txn$abbr.layer), ]
	rownames(txn) <- txn$abbr.layer
	txn <- txn[order(txn$layer), ]
	res$tab <- res$tab[rownames(txn), ]
}

#	drop latex file
tex <- as.data.frame(as.matrix(res$tab),
	stringsAsFactors = FALSE)

txn <- DecomposeNames(object, verbose = FALSE) 
txn <- txn[match(rownames(tex), txn$abbr.layer),]

tex <- data.frame(taxon = txn$taxon, layer = txn$layer, tex,
	stringsAsFactors = FALSE)
	
#	add blank lines and pointer to seperate diagnostic species
#	test if any group has no typical species
#	test for partitions without typical species
untyp <- unlist(typ) == "Nothing particularly typical"
tex.typical <- tex[match(unlist(typ)[!untyp], rownames(tex)), ]
tex.others <- tex[-match(unlist(typ)[!untyp], rownames(tex)), ]

#	block of typical species
tex.typical.seperated <- c()
for (i in c(1:nc)) {#[!typ == "Nothing particularly typical"]
#	i = 6
	sel <- match(typ[[i]], rownames(tex.typical))
	if (!any(is.na(sel))) {
		tmp <- tex.typical[sel[rep(1,2)], ]
		rownames(tmp) <- c(i, paste("typical", i, sep =""))
		tmp[1,1] <- ""
		tmp[2,1] <- paste("\\textbf{typical for ", i, "}", sep = "")	
		tmp[1:2, 2:ncol(tmp)] <- ""
		tmp <- rbind(tmp, tex.typical[sel, ])
	} else {
		tmp <- tex.typical[rep(1,2),]
		rownames(tmp) <- c(i, paste("typical", i, sep =""))
		tmp[1,1] <- ""
		tmp[2,1] <- paste("\\textbf{Nothing particularly typical for ", i, "}", sep = "")	
		tmp[1:2, 2:ncol(tmp)] <- ""
		#tmp[tmp == ""] <- "0"	

	}
	tex.typical.seperated <- rbind(tex.typical.seperated, tmp)

}

#	block of remining species, not typical for a particular partition
tex.others.seperated <- tex.others[1,]
rownames(tex.others.seperated) <- "others"
tex.others.seperated[1,1] <- "\\textbf{not particular typical}"
tex.others.seperated[1, 2:ncol(tex.others.seperated)] <- ""

empty.line <- tex.others.seperated[0,]
empty.line[1,] <- ""
rownames(empty.line) <- "0"

tex.others.seperated <- rbind(empty.line, tex.others.seperated, tex.others)

tex <- rbind(tex.typical.seperated, tex.others.seperated)

#	column widths and column names
p.col <- paste("p{", p.col.width, "}", sep = "")
col.just <- c("p{80mm}", "p{10mm}", rep(p.col, getK(object)))
col.names <- c("Taxon", "Layer", 1:getK(object))

if (length(Layers(object)) < 2) {
	tex <- tex[,-2]
	col.just <- col.just[-2]
	col.names <- col.names[-2]
	add2caption  <- paste("All species in the same layer ",
		Layers(object),
		". ",
		"Fidelity measure: ", object@method, ". ",
		sep = "")
} else {
	add2caption  <- ""
}

caption <- paste("Fidelity table for ",
		getK(object),
		" partitions. ",
		add2caption,
		"Statistics threshold: ", stat.min, ". ",
		"Relevees per partition: ",
		paste(names(table(Partitioning(object))),
			table(Partitioning(object)), sep = ":", collapse = ", "),
		". ",
		sep = "")

		
names(tex) <- col.names
tex <- as.matrix(tex)
tex[tex == 0] <- "."

#	move rare species to table footer
footer.species <- row.names(cntn)[rowSums(cntn) < footer.treshold]
#	check if we loose the only typical species in a partition
candidates <- footer.species[match(unlist(typ), footer.species, nomatch = 0)]
#	drop candidates from vector of footer species
for (i in seq(along = typ)[!typ == "Nothing particularly typical"]) {
	if (length(typ[[i]]) == 1 & any(!is.na(match(typ[[i]], footer.species)))) {
		footer.species <- footer.species[-match(candidates[match(typ[[i]], candidates)], footer.species)]
	} 
}
#	prune footer species and collapse to string 
tex.footer <- tex[match(footer.species, row.names(tex)), ]
tex <- tex[-match(footer.species, row.names(tex)), ]
footer <- cntn[match(row.names(tex.footer), row.names(cntn)), ]

txn <- DecomposeNames(object, verbose = FALSE)

txn <- txn[match(rownames(footer), txn$abbr.layer), ]
footer <- as.data.frame(footer, stringsAsFactors = FALSE)
footer$taxon <- txn$taxon
tmp <- c()

for (i in 1:nc) {
	tmp.i <- data.frame(footer[, i], footer$taxon)
	if (sum(tmp.i[,1]) > 0) {
		tmp.i <- paste("\\textbf{", i, "}: ",
			paste(tmp.i[tmp.i[, 1] != 0,][, 2], collapse = ", "), sep = "")	
		tmp <- c(tmp, tmp.i)
	}
}

#	nice language for low thresholds
if (footer.treshold < 4) {
	footer <- paste("\\textbf{Occuring only ", c("once", "twice", "thrice")[footer.treshold], " }",
		paste(tmp, collapse = "\n\n "), sep = "")
} else {
	footer <- paste("\\textbf{Occuring only ", footer.treshold, " times:}",
		paste(tmp, collapse = "\n\n "), sep = "")
}
footer <- paste("\\begin{multicols}{", molticols.footer, "}", footer, "\\end{multicols}")

#	remove rare species from list of typical species, if any
for (i in seq(along = typ)) {	
	tmp <- match(footer.species, typ[[i]])
	if (any(!is.na(tmp))) {
		tmp <- tmp[!is.na(tmp)]
		if (length(typ[[i]][-tmp]) < 1) {
			warning("list of typical species would be empty",
				" if this rare species gets dropped!")
			footer.species <- footer.species[-match(typ[[i]], footer.species)]
		} else {
			typ[[i]] <- typ[[i]][-tmp]
		}
	}
}

#	add boxes around diagnostic species
lab.cols <- max(grep("[[:alpha:]]", dimnames(tex)[[2]]))
cellTexCmds <- matrix(rep("", NROW(tex) * NCOL(tex)), nrow = NROW(tex))
for (i in c(1:nc) + lab.cols) {
#	i = 2
	sel <- match(typ[[i - lab.cols]], rownames(tex))
	cellTexCmds[sel, i] <- "\\multicolumn{1}{|l|}"
	tex[sel, i]  <- paste(cellTexCmds[sel, i], "{", tex[sel, i], "}", sep = "")
}

#	to do! see .latexVegsoupDataPartitionSites
#	more tests on filenames
if (length(grep(".", "_", filename, fixed = TRUE))) {
		
}

if (length(grep(" ", filename, fixed = TRUE)) > 0) {
	warning("LaTex demands no blanks in filenames!",
		" we replace all blanks!")
	filename <- gsub(" ", "_", filename, fixed = TRUE)	
}

if (length(grep(".tex", filename, fixed = TRUE)) < 1) {
	warning("add file extension .tex to filename ", filename)
	filename <- paste(filename, ".tex", sep = "")
}

#	times glyph in hybrid combinations
#	Taxon is always in first position in the table
tex[,1] <- gsub("×", "$\\times$", tex[,1], fixed = TRUE)
footer <- gsub("×", "$\\times$", footer, fixed = TRUE)

if (letters) {
	sel <- match(sort(unique(Partitioning(object))), dimnames(tex)[[2]])
	dimnames(tex)[[2]][sel] <- paste(dimnames(tex)[[2]][sel],
		LETTERS[sort(unique(Partitioning(object)))])
}
latex(tex,
	file = filename,
	caption = caption,
	rowname = NULL,
	booktabs = TRUE,
	longtable = TRUE,
	lines.page = nrow(tex),
	here = TRUE,
	col.just = col.just)

if (verbose) {
	cat("printed LaTex table to", filename)	
}

con <- file(filename, open = "a")
	writeLines(footer, con)
close(con)
if (verbose) {
	cat("appende footer to LaTex table in file", filename)	
}

return(invisible(res))
}

#	generic is set by VegsoupDataPartition-*Methods.R

#	may also be called for its side effect
setMethod("Latex",
	signature(object = "VegsoupDataPartitionFidelity"),
	.latexVegsoupDataPartitionFidelity	
)