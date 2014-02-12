#	Sites table
.latexVegsoupPartitionSites <- function (obj, choice = "sites", recursive = TRUE, file, ...) {
#	obj  <- fid

	sites <- Sites(obj)
	
	#	variables to drop for summary table	
	drop <- grep("date", names(sites), fixed = TRUE)
	drop <- c(drop, grep("longitude", names(sites), fixed = TRUE))
	drop <- c(drop, grep("latitude", names(sites), fixed = TRUE))
	
	file <- .texfile(file)
		
	if (length(drop) > 0) {
		#if (verbose) {
			message("dropped variables ",
			paste(names(sites)[drop], collapse = ", "),
			". not meaningful for summary")
		#}	
		sites <- sites[ ,-drop]
	}
#	if (missing(col.width)) {
		col.width <- "10mm"
#	}
	
	part <- Partitioning(obj)
	
	num.cols <- sapply(sites, is.numeric)
	str.cols <- sapply(sites, is.character)
	
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
	
	str.cols.agg <- matrix(NA,
		ncol = length(which(str.cols)),
		nrow = getK(obj))
		
	for (i in seq(along = which(str.cols))) {
		#	i = 1
		i.table <- data.frame(variable = sites[,which(str.cols)[i]], part)
		j.res <- c()
		for (j in 1:getK(obj)) {
			j.tmp <- table(i.table[i.table$part == j,]$variable)
			j.tmp <- sort(j.tmp[j.tmp > 0], decreasing = TRUE)
			j.res <- c(j.res, paste(names(j.tmp), j.tmp, sep = ":", collapse = ", "))
		}	
		str.cols.agg[,i] <- j.res
	}
	
	str.cols.agg <- as.data.frame(str.cols.agg, stringsAsFactors = FALSE)
	names(str.cols.agg) <- names(sites)[str.cols]
	
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
		num.cols.agg, str.cols.agg,
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
	
	latex(tex,
		file = file,
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
	
.latexVegsoupPartitionSitesRecursive <- function (obj, choice = "sites", recursive = TRUE, file, ...) {
	#	Suggests:
	require(Hmisc)
	#	to do!	
}

.latexVegsoupPartitionSpecies <- function (obj, file, mode, p.max, stat.min, constancy.min, taxa.width, col.width, footer.treshold, molticols.footer, use.letters, caption.text, quantile.select, coverscale, sep, sites.columns, newpage, verbose) {
	CALL <- match.call()
	#	Suggests:
	require(Hmisc)
	if (class(obj) != "VegsoupPartitionFidelity") {
			cat("apply default indicator species statistic")
		obj <- Fidelity(obj, ...)
	}
	
	#	inspired by Sebastian Schmidtlein's isotab() in package isopam
	
	ct <- contingency(obj)
	cs <- constancy(obj)
	nc <- getK(obj)
	sp <- ncol(obj)
	
	ft <- obj@fisher.test
	N  <- nrow(obj)
	
	frq <- colSums(obj)
	siz <- table(Partitioning(obj))

	file <- .texfile(file)
	
	if (getK(obj) > 10) {
		use.letters = TRUE
	}
	if (is.null(stat.min)) {
		message("here")
		if (obj@method == "r.g") {
			#	automatic guess adapted from isopam()
			stat.min = round(0.483709 + nc * -0.003272 + N * -0.000489 + sp * 0.000384 + sqrt (nc) * -0.01475, 2) 
		}
	}
	#	drop coordiantes
	drp <- c(
		grep("longitude", sites.columns), # already dropped in Vegsoup.R
		grep("latitude", sites.columns),  # already dropped in Vegsoup.R
		grep("precision", sites.columns)
	)
	#	drop all columns constant at zero
	drp.zeros <- which(apply(Sites(obj)[, sapply(Sites(obj), is.numeric)], 2, sum) == 0)
	drp <- c(drp, drp.zeros)
	sites.columns <- sites.columns[ -drp ]


	###	debug both MODE 1 & MODE 2
		
	###	init steps for mode 1 and 2
	#	significance symbols
	test <- apply(ft, 2, function (x) any(x <= 0.05))
	
	if (!all(test)) {
		message(paste("Not a single species beeing significant for cluster",
			as.vector(which(!test))))
	}
	
	symb <- ft
	symb[ft >  0.050] <- ""
	symb[ft <= 0.050] <- "*"
	symb[ft <= 0.010] <- "**"
	symb[ft <= 0.001] <- "***"
	
	#	combine frequency table with significance symbols
	frq.ft <- matrix(paste(cs, symb, sep = ""), 
		nrow = nrow(cs), ncol = ncol(cs))
	frq.ft <- data.frame(frq.ft)
	colnames(frq.ft) <- 1:nc
	rownames(frq.ft) <- colnames(obj)
	  
	#	fidelity measure
	stat <- getStat(obj)
	
	#	sort table
	# 	which cluster has highest fidelity
	stat.idx <- apply(stat, 1, which.max)
	frq.ord <- stat.idx
	
	for (i in 1:length(frq.ord)) {
		frq.ord[i] <- cs[i, stat.idx[i]]
	}	
	
	#	sort frequency table
	#	first by fidelity measure and then by constancy
	frq.top <- as.matrix(frq)[order(stat.idx, -frq.ord), ]
	ord.top <- names(frq.top)
	frq.ft.top <- frq.ft[ord.top, ]
	ft <- ft[ord.top, ]
	stat <- stat[ord.top, ]
	
	#	filter diagnostic species
	dia <- which(c(apply(ft, 1, min) <= p.max)[c(apply(stat, 1, max) >= stat.min)] == TRUE)
	
	if (length(dia) == 0) {
		diag <- "No diagnostic species with given thresholds." 
	}	
	
	if (length(dia) > 0) {
		diag <- frq.ft.top[names(dia), ]
	}	
	
	#	for later use in the bottom part of the tables
	ord.bot <- names(as.matrix(frq)[order(-frq), ])
	frq.ft.b <- frq.ft[ord.bot, ]
	    
	#	move diagnostic species to top
	if (length(dia) > 0) {
		tmp <- rbind(diag,
			frq.ft.b[rownames(frq.ft.b) %in% rownames(diag) == FALSE, ])
	} else {
		tmp <- frq.ft.b
	}
	
	#	info about diagnostic species
	dig1 <- stat.idx[names(stat.idx) %in% names(dia)]
	dig2 <- dig1[rownames(diag)]
	typ <- list ()
	
	for (i in 1:nc) {
		if (length(names(dig2)[dig2 == i]) > 0) {
			typ[[i]] <- c(names(dig2)[dig2 == i])
		} else {
			typ[[i]] <- "Nothing particularly typical"
		}	
	}
	names(typ) <- colnames(cs)
	
	tmp <- list(tab = tmp, typical = typ)
	
	#	top and bottom of table
	if (length(dia) > 0) {
		#	top of table, diagnostic/typical species
		txn <- splitAbbr(obj)
		txn <- txn[match(rownames(tmp$tab), rownames(txn)), ]
		#	txn <- txn[rownames(tmp$tab), ]
		#	rownames(txn) <- txn$abbr.layer
		txn.top <- txn[rownames(diag), ]
		tmp.i <- c()
		for (i in Layers(obj)) {
			tmp.i <- rbind(tmp.i, txn.top[txn.top$layer == i, ])
		}
		txn.top <- tmp.i
		top <- tmp$tab[rownames(txn.top), ]
		
		#	bottom of table, remaining species
		txn.bottom <- txn[-match(rownames(diag), rownames(tmp$tab)), ]
		tmp.i <- c()
		for (i in Layers(obj)) {
			tmp.i <- rbind(tmp.i, txn.bottom[txn.bottom$layer == i, ])
		}
		txn.bottom <- tmp.i
		bottom <- tmp$tab[rownames(txn.bottom), ]
		tmp$tab <- rbind(top, bottom)
	} else {
		txn <- splitAbbr(obj)
		txn <- txn[match(rownames(tmp$tab), rownames(txn)), ]
		#rownames(txn) <- txn$abbr.layer
		txn <- txn[order(txn$layer), ]
		tmp$tab <- tmp$tab[rownames(txn), ]
	}
	
	#	create intermediate results
	tex <- as.data.frame(as.matrix(tmp$tab),
		stringsAsFactors = FALSE)
	
	txn <- splitAbbr(obj) 
	txn <- txn[match(rownames(tex), rownames(txn)), ]
	
	tex.out <- tex <- data.frame(taxon = txn$taxon, layer = txn$layer, tex,
		stringsAsFactors = FALSE, check.names = FALSE)

	
	###	internal funtion branching for MODE 1
	#	standard mode, all species in one table
	#	add typesetting commands to intermediate results backuped as tex.out
	
	if (mode == 1) {
		
		#	add blank lines and pointer to seperate diagnostic species
		#	test if any group has no typical species
		#	test for partitions without typical species
		untyp <- unlist(typ) == "Nothing particularly typical"
		tex.typical <- tex[match(unlist(typ)[!untyp], rownames(tex)), ]
		tex.others <- tex[-match(unlist(typ)[!untyp], rownames(tex)), ]
	
		#	block of typical species
		tex.typical.seperated <- c()
		for (i in c(1:nc)) {
			#[!typ == "Nothing particularly typical"]
			#	i = 6
			sel <- match(typ[[i]], rownames(tex.typical))
			if (!any(is.na(sel))) {
				tmp <- tex.typical[sel[rep(1,2)], ]
				rownames(tmp) <- c(i, paste("typical", i, sep =""))
				tmp[1, 1] <- ""
				tmp[2, 1] <- paste("\\textbf{typical for ", i, "}", sep = "")	
				tmp[1:2, 2:ncol(tmp)] <- ""
				tmp <- rbind(tmp, tex.typical[sel, ])
			} else {
				tmp <- tex.typical[rep(1,2),]
				rownames(tmp) <- c(i, paste("typical", i, sep =""))
				tmp[1, 1] <- ""
				tmp[2, 1] <-
					paste("\\textbf{Nothing particularly typical for ", i, "}",sep = "")	
				tmp[1:2, 2:ncol(tmp)] <- ""
			}
		tex.typical.seperated <- rbind(tex.typical.seperated, tmp)
		}
	
		#	block of remaining species, not typical for a particular cluster
		tex.others.seperated <- tex.others[1,]
		rownames(tex.others.seperated) <- "others"
		tex.others.seperated[1, 1] <- "\\textbf{not particular typical}"
		tex.others.seperated[1, 2:ncol(tex.others.seperated)] <- ""
	
		empty.line <- tex.others.seperated[0,]
		empty.line[1, ] <- ""
		rownames(empty.line) <- "0"
	
		tex.others.seperated <- rbind(empty.line, tex.others.seperated, tex.others)
	
		tex <- rbind(tex.typical.seperated, tex.others.seperated)
	
		#	column widths and column names
		p.col <- paste("p{", col.width, "}", sep = "")
		col.just <- c(paste("p{", taxa.width, "}", sep = ""), "p{10mm}",
			rep(p.col, getK(obj)))
		col.names <- c("Taxon", "Layer", 1:getK(obj))
	
		if (length(Layers(obj)) < 2) {
			tex <- tex[, -2]
			col.just <- col.just[-2]
			col.names <- col.names[-2]
			add2caption  <- paste("All species in the same layer ",
				Layers(obj),
				". ",
				"Fidelity measure: ", obj@method, ". ",
				sep = "")
		} else {
			add2caption  <- ""
		}
	
		#	table caption
		caption <- paste("Fidelity table for ",
			getK(obj),
			" partitions. ",
			add2caption,
			"Statistics threshold: ", stat.min, ". ",
			"Relevees per partition: ",
			paste(names(table(Partitioning(obj))),
				table(Partitioning(obj)), sep = ":", collapse = ", "),
			". ",
			sep = "")
			# additional user supplied text
			caption <- paste(caption, caption.text, collapse = " ")	
	
			#	prepare intermediate result for formating	
			names(tex) <- col.names
			tex <- as.matrix(tex)
			tex[tex == 0] <- "."
	
			#	move rare species to table footer
			footer.species <- row.names(ct)[rowSums(ct) < footer.treshold]
	
			#	check if we loose the only typical species in a partition
			candidates <- footer.species[match(unlist(typ), footer.species, nomatch = 0)]
			
			#	for data set with very low species diversity try to reduce footer treshold
			#	omit footer and raise a warning
	
		if (length(candidates) > 0) {
		#	drop candidates from vector of footer species
			for (i in seq(along = typ)[!typ == "Nothing particularly typical"]) {
				if (length(typ[[i]]) == 1 & any(!is.na(match(typ[[i]], footer.species)))) {
					footer.species <- footer.species[-match(candidates[match(typ[[i]], candidates)], footer.species)]
				} 
			}
	
			#	prune footer species and collapse to string 
			tex.footer <- tex[match(footer.species, row.names(tex)), ]
			tex <- tex[-match(footer.species, row.names(tex)), ]
			footer <- ct[match(row.names(tex.footer), row.names(ct)), ]
	
			txn <- splitAbbr(obj)
			txn <- txn[match(rownames(footer), rownames(txn)), ] #	dropped $abbr.layer
			footer <- as.data.frame(footer, stringsAsFactors = FALSE)
			footer$taxon <- txn$taxon
			tmp <- c()
	
			for (i in 1:nc) {
				tmp.i <- data.frame(footer[, i], footer$taxon)
				if (sum(tmp.i[,1]) > 0) {
					tmp.i <- paste("\\textbf{", i, "}: ",
						paste(tmp.i[tmp.i[, 1] != 0,][, 2], collapse = ", "),
						sep = "")	
						tmp <- c(tmp, tmp.i)
					}
			}
	
			#	nice language for low thresholds
			if (footer.treshold < 4) {
				footer <- paste("\\textbf{Occuring only ",
					c("once", "twice", "thrice")[footer.treshold], " }",
					paste(tmp, collapse = "\n\n "), sep = "")
				}
				else {
				footer <- paste("\\textbf{Occuring only ",
					footer.treshold, " times:}",
					paste(tmp, collapse = "\n\n "), sep = "")
			}
			footer <- paste("\\begin{multicols}{", molticols.footer, "}",
				footer, "\\end{multicols}")
		}
		else {
			message("\nfooter is empty with given treshold: ",
				footer.treshold, "!")
			footer <- ""
		}
	
		#	remove rare species from list of typical species, if any
		for (i in seq(along = typ)) {	
			tmp <- match(footer.species, typ[[i]])
			if (any(!is.na(tmp))) {
				tmp <- tmp[!is.na(tmp)]
				if (length(typ[[i]][-tmp]) < 1) {
					message("list of typical species would be empty",
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
			#	i = 5
			sel <- match(typ[[i - lab.cols]], rownames(tex))
			#	if no species is significant 
			if (!any(is.na(sel))) {
				cellTexCmds[sel, i] <- "\\multicolumn{1}{|l|}"
			tex[sel, i]  <- paste(cellTexCmds[sel, i], "{", tex[sel, i], "}", sep = "")
			}
		}
		tex.out <- tex
		footer.species <- footer
		footer.sites <- NULL
	} # end if (mode == 1)
	
	###	internal function branching for MODE 2
	#	partition summary
	#	add typesetting commands to intermediate results backuped as tex.out
	
	if (mode == 2) {
		if (verbose) message("\nrun mode ", mode)
		
		#	species summary
		fn <- quantile(obj, coverscale = coverscale)
		fn <- fn[match(rownames(tex.out), rownames(fn)), ,]
		#	groome names
	
		fn.m <- t(apply(fn, c(2, 1),
			function (x) {
				res <- x[quantile.select]
				res <- paste(res, collapse = sep)
				res
			}))
	
			
		#	sites summary	
		sts <- Sites(obj)
		sts$part <- Partitioning(obj)
		sts <- sts[, c("part", sites.columns)]
	
		#	resort to match order of intermediate result table  
		fn <- fn[match(rownames(tex), rownames(fn)), , ]
		fn.m <- fn.m[match(rownames(tex), rownames(fn.m)), ]
		cs <- cs[match(rownames(tex), rownames(cs)), ]
		ct <- ct[match(rownames(tex), rownames(ct)), ]
		stat <- stat[match(rownames(tex), rownames(stat)), ]
		sprd <- spread(obj) # speed issue, method is slow!
		sprd <- sprd[match(rownames(tex), names(sprd))]

		#	identical(rownames(fn), rownames(fn.m))
		#	identical(rownames(fn), rownames(cs))
		#	identical(rownames(fn), rownames(ct))
		#	identical(rownames(fn), rownames(stat))				
		#	identical(rownames(fn), names(sprd))
					
		tex <- footer.sites <- footer.species <- vector("list", length = getK(obj))
		names(tex) <- names(footer.sites) <- names(footer.species) <- 1:getK(obj)
		
		for (i in 1:getK(obj)) {
			#	i = 1
			sel <- which(cs[, i] > 0)
			
			#	main table
			tmp <- data.frame(
			#	abbr.layer = rownames(cs[sel, ]),
				typical = "",
				stat = round(stat[sel ,i], 3),
				cons = cs[sel, i],
				cont = ct[sel, i],
				occu = 0,
				out = 0,
				spread = 0,
				fn[sel, i, ],
				summary = paste(cut(cs[sel, i],
				breaks = seq(0, 100, by = 20),
					labels = c("I","II", "III", "IV", "V")),
					" (", fn.m[sel, i], ", n = ", ct[sel, i], ")", sep = ""),
				stringsAsFactors = FALSE)
			
			tmp$typical[match(typ[[i]], rownames(tmp))] <- "yes"
			tmp <- tmp[order(-tmp$stat, tmp$cons),]
			tmp$occu <- sapply(sprd[match(rownames(tmp), names(sprd))],
				function (x) length(x))
			tmp$spread <- sapply(sprd[match(rownames(tmp), names(sprd))],
				function (x) length(unique(x)))
			tmp$out <- tmp$occu - tmp$cont
				
			tmp <- cbind(txn[match(rownames(tmp), rownames(txn)), c("taxon", "layer")], tmp,
					row.names = rownames(tmp))
			tex[[i]] <- tmp[tmp$cont > footer.treshold | tmp$typical == "yes", ]
			
			#	rare species footer
			tmp <- tmp[tmp$cont <= footer.treshold & tmp$typical != "yes", ]
			tmp <- data.frame(parameter = paste("Occuring only ",
				c("once", "twice", "thrice")[footer.treshold], sep = ""),
				values = paste(sort(tmp$taxon), collapse = ", "), stringsAsFactors = FALSE)
			footer.species[[i]] <- tmp	
			
			#	sites summary footer		
			tmp <- sts[sts$part == i, -1]
			num <- apply(tmp, 2, function (x) is.numeric(type.convert(x)))
		
			#	string varibales			
			tmp.str <- apply(tmp[, !num, drop = FALSE], 2, table)
			tmp.str <- sapply(tmp.str, function (x) x[order(x, decreasing = TRUE)])
			tmp.str <- sapply(tmp.str, function (x) paste(names(x), x, sep = ": ", collapse = "; "))
			#tmp.str <- sapply(tmp.str, function (x) as.vector(x))

			#	numerical variables
			#	fivenum can be critical!
			tmp.num <- sapply(as.data.frame(
				apply(tmp[, num], 2, function (x) fivenum(x, na.rm = TRUE))), list)
			#	reduce constant varibales
			sel <- sapply(tmp.num, function (x) length(unique(x))) <= 1			
			tmp.num[names(sel)[sel]] <- "."
			tmp.num <- sapply(tmp.num, function (x) {
				if (length(x) == 5) {# median of fivenum bold
					x[3] <- paste("\\textbf{", x[3], "}")
				}	
				paste(x, collapse = "/")
				}, simplify = FALSE)
			
			tmp <- as.matrix(c(tmp.num, tmp.str))
			
			tmp <- data.frame(parameter = unlist(names(tmp[,1])),
				values = unlist(tmp[,1]), stringsAsFactors = FALSE)
			
			#	& glyph that might be used in Sites(obj)
			sel <- which(apply(tmp, 2, function (x) (length(grep("&", x)) > 0)))
			if (length(sel) > 0) {
				for (j in sel) {
					tmp[, j] <- gsub("&", "\\&", tmp[,j ], fixed = TRUE)			
				}  
			}
			footer.sites[[i]] <- tmp
		}
		tex.out <- tex
	}
	# end if (mode == 2)
	
	if (mode == 1) {
		if (verbose) message("run mode", mode)
		#	check species characters
		#	times glyph in hybrid combinations
		#	taxon is always in first position in the table
		tex[, 1] <- gsub("\u00D7", "$\\times$", tex[, 1], fixed = TRUE)
		footer <- gsub("\u00D7", "$\\times$", footer, fixed = TRUE)
		#	& gylph used in Sites(obj)
		footer <- gsub("&", "\\&", footer, fixed = TRUE)
		footer <- gsub("\u00D7", "$\\times$", footer, fixed = TRUE)
		tex <- gsub("%", "\\%", tex, fixed = TRUE)
		tex <- gsub("&", "\\&", tex, fixed = TRUE)
		tex <- gsub("\u00D7", "$\\times$", tex, fixed = TRUE)
		
		if (use.letters) {
			sel <- match(sort(unique(Partitioning(obj))), dimnames(tex)[[2]])
			dimnames(tex)[[2]][sel] <- paste(dimnames(tex)[[2]][sel],
				" (", LETTERS[sort(unique(Partitioning(obj)))], ")", sep = "")
		}
		if (verbose) {
			cat("\nprint LaTex table to", file)	
		}

		latex(tex,
			file = file,
			caption = caption,
			rowname = NULL,
			booktabs = TRUE,
			longtable = TRUE,
			lines.page = nrow(tex),
			here = TRUE,
			col.just = col.just)
	
		# append footer to LaTex table in file
	
		con <- file(file, open = "a")
			writeLines(footer, con)
		close(con)
	}
	# end if (mode == 1)
	
	if (mode == 2) {
	#	check species characters
	#	times glyph in hybrid combinations	
		tex.out <- sapply(tex.out, function (x) {
				tmp <- x
				#	replace \u00D7
				tmp[, 1] <- gsub("\u00D7", "$\\times$", tmp[, 1], fixed = TRUE)
				#	make taxa having cons >= a user defined constancy treshold
				#	check first if we have a singleton
				if (any(tmp[, 5] < 100)) {
				tmp[tmp[, 5] >= constancy.min, 1] <- 
					paste("\\textbf{", tmp[tmp[, 5] >= constancy.min, 1], "}")
				}	
				tmp
				}, simplify = FALSE)
		
		footer.species <- sapply(footer.species, function (x) {
				tmp <- x
				#	replace \u00D7
				tmp[, 2] <- gsub("\u00D7", "$\\times$", tmp[, 2], fixed = TRUE)
				tmp
				}, simplify = FALSE)
			
		#	create file for appending	
		con <- file(file)
			writeLines("%start", con)
		close(con)	
	
		for (i in 1:getK(obj)) {
			latex(as.matrix(tex.out[[i]]),
				file = file,
				append = TRUE,
				caption = paste("Partion summary for cluster ", i,
					" consisting out of ", table(Partitioning(obj))[i], " plots.",
					ifelse(length(caption.text) > 0, paste(" ", caption.text, ".", sep = ""), ""),
					sep = ""),
				rowname = NULL,
				booktabs = TRUE,
				longtable = TRUE,
				lines.page = nrow(tex.out[[i]]),
	#			numeric.dollar = FALSE, # raises errors in format.df
				here = TRUE
			)
	
		con <- file(file)
			tmp <- readLines(con)
			hook <- max(grep("bottomrule", tmp))
	
			tmp.bgn <- 1: c(hook -1) # begin
			tmp.end <- hook:length(tmp) # end
			
			tmp.ins1 <- footer.species[[i]] # insert 1
			tmp.ins1 <- apply(tmp.ins1, 1, function (x) {
				paste(x[1], "& \\multicolumn{",
					dim(tex.out[[i]])[2] - 1, "}",
					"{p{", footer.width, "}}",
					"{", x[2], "}", "\\tabularnewline", sep = "")	
			})
			tmp.ins2 <- footer.sites[[i]] # insert 2
			tmp.ins2 <- apply(tmp.ins2, 1, function (x) {
				paste(x[1], "& \\multicolumn{",
					dim(tex.out[[i]])[2] - 1, "}",
					"{p{", footer.width, "}}",
					"{", x[2], "}", "\\tabularnewline", sep = "")	
			})
			
			tmp <- c(tmp[tmp.bgn], "\\midrule", tmp.ins1, "\\midrule", tmp.ins2, tmp[tmp.end])
			
			if (newpage) {
				tmp <- c(tmp, "\n\\newpage")
			}
			writeLines(tmp, con)
		close(con)		
		}
			
}
	# end if (mode == 2)
	return(invisible(list(
		table = tex.out, footer.sites = footer.sites,
		footer.species = footer.species)))
}

#	\dots passed to seriation()
.latexVegsoupPartitionSpeciesRecursive <- function (obj, choice = "species", recursive = TRUE, file, col.width, taxa.width, caption.text, verbose, ...) {
	#	Suggests:
	require(Hmisc)
		
	#	obj  <- prt
	if (missing(file)) {
		message("no path supplied for LaTex files")
	}	
	if (missing(verbose)) {
		verbose = FALSE
	}
	if (missing(col.width)) {
		col.width <- "10mm"
		if (verbose) {
			message("col.width missing, set to ", col.width)
		}	
	}	
	if (missing(taxa.width)) {
		taxa.width <- "60mm"
		if (verbose) {
			message("col.width missing, set to ", taxa.width)
		}	
	}
	if (missing(caption.text)) {
		caption.text <- ""
		if (verbose) {
			message("caption.text missing, set to", caption.text)
		}	
	}
	
	res <- vector("list", length = getK(obj))
	files <- c()
	
	for (i in 1:getK(obj)) {
		#	obj = prt; i = 2
		i.part <- obj[Partitioning(obj) == i, ]
		i.part <- seriation(i.part, ...)
		#	table will be order according to Layers(obj)
		
		res[[i]] <- i.part
		
		i.tex <- t(as.character(i.part))
		i.tex <- gsub("0", ".", i.tex, fixed = TRUE)
	
		i.tex <- cbind(splitAbbr(i.part)[c("taxon", "layer")], i.tex)
		#	tex valid files
		file <- paste(file, "species", i, ".tex", sep = "")
		files <- c(files, file)
		caption <- paste("Sample table of Cluster", i)
	
		p.col <- paste("p{", col.width, "}", sep = "")
		col.just <- c(paste("p{", taxa.width, "}", sep = ""), "p{10mm}",
			rep(p.col, dim(i.part)[1]))
		
		col.heads = c("Taxon", "Layer", paste("\\rotatebox{90}{", dimnames(i.tex)[[2]][-c(1,2)], "}"))
		
		latex(i.tex,
		file = file,
		caption = paste(caption, caption.text, collapse = " "),
		rowname = NULL,
		booktabs = TRUE,
		longtable = TRUE,
		lines.page = nrow(i.tex),
		here = TRUE,
		col.just = col.just,
		colheads = col.heads) 
	}
	
	con <- file(paste(file, "species.tex", sep = ""))
		writeLines(paste("\\input{",
				gsub(file, "", files, fixed = TRUE),
				"}", sep = ""), con)
	close(con)
	
	return(invisible(res))
}

#	if(!isGeneric("Latex")) {
setGeneric("Latex",
	function (obj, choice = "species", recursive = FALSE, file, mode = 1, p.max = .05, stat.min = NULL, constancy.min = 95, taxa.width = "60mm", col.width = "5mm", footer.width = "150mm", footer.treshold = 1, molticols.footer = 2, use.letters = FALSE, caption.text = NULL, quantile.select = c(1,3,5), coverscale = FALSE, sep = "/", sites.columns = names(obj), newpage = TRUE, verbose = FALSE, ...)
		standardGeneric("Latex")
)
#}

#	all defaults inhertited from generic!
setMethod("Latex",
	signature(obj = "VegsoupPartition"),
	function (obj, ...) {
			#	Suggests:		
			require(Hmisc)
			CALL <- match.call()

			CHOICES <- c("species", "sites")
			choice <- CHOICES[pmatch(choice, CHOICES)]
			if (is.na(choice)) stop("choice must be either species or sites")
						
			stopifnot(is.logical(recursive))

			if (choice == "sites" & !recursive) {
				if (missing(file)) file = "SitesPartitionTable"
				res <- .latexVegsoupPartitionSites(obj, file = file, ...)
			}
			if (choice == "species" & !recursive) {
				if (missing(file) & mode == 1) file = "FidelityTable"
				if (missing(file) & mode == 2) file = "PartitionSummary"
				res <- .latexVegsoupPartitionSpecies(obj, file = file,
					mode = mode, p.max = p.max, stat.min = stat.min,
					constancy.min = constancy.min, taxa.width = taxa.width,
					col.width = col.width, footer.treshold = footer.treshold,
					molticols.footer = molticols.footer, use.letters = use.letters,
					caption.text = caption.text, quantile.select = quantile.select,
					coverscale = coverscale, sep = sep, sites.columns = sites.columns,
					newpage = newpage, verbose = verbose, ...)
			}
			if (choice == "sites" & recursive) {
				if (missing(file)) file = "SitesTables"
				res <- .latexVegsoupPartitionSitesRecursive(obj, file = file, ...)
			}
			if (choice == "species" & recursive) {
				if (missing(file)) file = "SpeciesTables"				
				res <- .latexVegsoupPartitionSpeciesRecursive(obj, file = file, ...)
			}			
	return(invisible(res))
	}
)