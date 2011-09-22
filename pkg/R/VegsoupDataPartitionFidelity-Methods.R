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
.latexVegsoupDataPartitionFidelity <- function (object, p.col.width, p.max, filename, stat.min, footer.treshold, molticols.footer, verbose = FALSE, ...) {
#	object = fid.prt
if (missing(filename)) {
	filename <- paste("FidelityTable")
}
if (missing(p.col.width)) {
	p.col.width = "10mm"
	warning("p.col.width missing, set to ", p.col.width, call. = FALSE)
}
if (missing(p.max)) {
	p.max = .05
	warning("p.max missing, set to ", p.max, call. = FALSE)
}
if (missing(footer.treshold)) {
	footer.treshold = 2
}
if (missing(molticols.footer)) {
	molticols.footer = 3
}
cntn <- Contingency(object)
cnst <- Constancy(object)
nc <- ncol(cnst)
sp <- ncol(object)

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
	txanames <- DecomposeNames(object, verbose = FALSE)
	txanames <- txanames[match(rownames(res$tab), txanames$abbr.layer), ]
	rownames(txanames) <- txanames$abbr.layer
	txanames.top <- txanames[rownames(diag), ]
	txanames.top <- txanames.top[order(txanames.top$layer),]
	top <- res$tab[rownames(txanames.top), ]
	
	#	bottom of table, remaining species
	txanames <- DecomposeNames(object, verbose = FALSE)
	txanames <- txanames[match(rownames(res$tab), txanames$abbr.layer), ]
	rownames(txanames) <- txanames$abbr.layer
	txanames.bottom <- txanames[-match(rownames(diag), rownames(res$tab)), ]
	txanames.bottom <- txanames.bottom[order(txanames.bottom$layer),]
	bottom <- res$tab[rownames(txanames.bottom), ]
	res$tab <- rbind(top, bottom)
} else {
	txanames <- DecomposeNames(object, verbose = FALSE)
	txanames <- txanames[match(rownames(res$tab), txanames$abbr.layer), ]
	rownames(txanames) <- txanames$abbr.layer
	txanames <- txanames[order(txanames$layer), ]
	res$tab <- res$tab[rownames(txanames), ]
}

#	drop latex file
tex <- as.data.frame(as.matrix(res$tab),
	stringsAsFactors = FALSE)

txanames <- DecomposeNames(object, verbose = FALSE) 
txanames <- txanames[match(rownames(tex), txanames$abbr.layer),]

tex <- data.frame(taxon = txanames$taxon, layer = txanames$layer, tex,
	stringsAsFactors = FALSE)
	
#	add blank lines and pointer to seperate diagnostic species
tex.typical <- tex[match(unlist(typ), rownames(tex)), ]
tex.others <- tex[-match(unlist(typ), rownames(tex)), ]

#	block of typical species
tex.typical.seperated <- c()
for (i in 1:nc) {
#	i = 1
	sel <- match(typ[[i]], rownames(tex.typical))
	tmp <- tex.typical[sel[rep(1,2)], ]
	rownames(tmp) <- c(i, paste("typical", i, sep =""))
	tmp[1,1] <- ""
	tmp[2,1] <- paste("\\textbf{typical for ", i, "}", sep = "")	
	tmp[1:2, 2:ncol(tmp)] <- ""
	tex.typical.seperated <- rbind(tex.typical.seperated,
		rbind(tmp, tex.typical[sel, ]))
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
tex.footer <- tex[match(footer.species, row.names(tex)), ]
tex <- tex[-match(footer.species, row.names(tex)), ]
footer <- cntn[match(row.names(tex.footer), row.names(cntn)), ]

txanames <- DecomposeNames(object, verbose = FALSE)

txanames <- txanames[match(rownames(footer), txanames$abbr.layer), ]
footer <- as.data.frame(footer, stringsAsFactors = FALSE)
footer$taxon <- txanames$taxon
tmp <- c()

for (i in 1:nc) {
	tmp.i <- data.frame(footer[, i], footer$taxon)
	tmp.i <- paste("\\textbf{", i, "}: ",
		paste(tmp.i[tmp.i[, 1] != 0,][, 2], collapse = ", "), sep = "")	
	tmp <- c(tmp, tmp.i)
}

footer <- paste("\\textbf{Occuring only once:}", paste(tmp, collapse = "\n\n "))
footer <- paste("\\begin{multicols}{", molticols.footer, "}", footer, "\\end{multicols}")

#	remove rare species from list of typical species, if any
for (i in seq(along = typ)) {	
	tmp <- match(footer.species, typ[[i]])
	if (any(!is.na(tmp))) {
		tmp <- tmp[!is.na(tmp)]
		typ[[i]] <- typ[[i]][-tmp]
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
#	more tests on filename
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