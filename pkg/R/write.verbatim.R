#	complements verbatim() in Vegsoup-Import.R
.write.verbatimVegsoup <- function (obj, file, select, absence = ".", sep = " ", pad = 1, abbreviate = TRUE, short.names = FALSE, rule, add.lines = FALSE, latex.input = FALSE, table.nr = FALSE) {

	if (class(obj) != "Vegsoup" & class(obj) != "VegsoupPartition") {
		stop("verbatim is currently only implemented for class Vegsoup and VegsoupPartition?")
	}
	
	if (class(obj) == "VegsoupPartition") {
		#	obj = prt
		obj <- obj[order(Partitioning(obj)), ]
	}
	if (missing(file)) {
		no.save <- TRUE
		message("no filename provided")
	}
	else {
		no.save <- FALSE
		file <- file.path(file)
	}
	if (missing(select)) {
		message("think about selecting sites varibales? ",
			"try to use all numeric columns")
		#	select all numeric columns
		tmp <- as.matrix(Sites(obj))
		mode(tmp) <- "character"
		select <- apply(tmp, 2,
			function (x) is.numeric(type.convert(x, as.is = FALSE)))
		if (all(select) == FALSE) {
			message("found no numeric columns?")
			obj$richness <- richness(obj, "sample")
			select <- select
		}	
	}
	else {
		if (is.numeric(select)) {
			if (any(is.na(names(obj)[select]))) {
				stop("select must match columns in Sites(obj)")
			}
		}
		if (is.character(select)) {
			select <- match(select, names(obj))
			if (any(is.na(select))) {
				stop("select must match columns in Sites(obj)")
			}
		}
	}
	if (missing(rule) & class(obj) != "VegsoupPartition") {
		rule <- FALSE
	}
	else {
		if (class(obj) == "Vegsoup") {
			stopifnot(length(rule) == nrow(obj))
			rule.col <- cumsum(rle(rule)$lengths)
			rule <- TRUE
		}	
		if (class(obj) == "VegsoupPartition" & missing(rule)) {
			rule.col <- cumsum(rle(Partitioning(obj))$lengths)
			rule <- TRUE
		}	
	}
	
	if (table.nr) {
		rownames(obj) <- sprintf(paste0("%0", nchar(nrow(obj)), "d"), 1:nrow(obj))
	}
	
	singleton <- nrow(obj) == 1
	
	if (singleton) {
		res <- relevee(obj, 1, format = TRUE)
	}
	else {	
		#	width of layer codes
		nchar.layer <- max(sapply(Layers(obj), nchar))
		
		m <- as.matrix(obj, typeof = "character", mode = "R")
		txa <- splitAbbr(obj)[rownames(m), ]
		
		#	prepare species data block
		if (short.names) {
			taxon <- gsub(".", " ", txa$abbr, fixed = TRUE)
		} else {
			taxon <- txa$taxon
		}
		#	pad space to taxa (right) and layer (both sides)
		#	this also ensures equal widths 
		taxon <- str_pad(taxon, max(sapply(taxon, nchar)) + pad, "right")
		layer <- str_pad(str_pad(txa$layer, nchar.layer + pad, "left"),
						nchar.layer + (2 * pad), "right")
		#	recode absences
		m <- gsub("0", absence, m, fixed = TRUE)
		
		#	species data block
		x <- cbind(taxon, layer, m)
		
		#	truncate abundance value
		if (abbreviate & is.ordinal(obj)) {
			width <- max(sapply(coverscale(obj)@codes, nchar))
			if (width > 1) {
				x[, -c(1,2)] <- apply(x[, -c(1,2)], 2,
					function (x) abbreviate(x, minlength = 1, strict = TRUE)
				)
			}
		}
		#	sparse layer annotation
		x[duplicated(x[,2]), 2] <- format("", width = nchar(layer[1]))
		
		#	sites ('attributes') data block
		y <- t(Sites(obj)[, select, drop = FALSE]) #names(obj)[select]
		labels <- rownames(y)
		m <- vector("list", length(labels))
		names(m) <- labels
		
		for (i in 1:length(labels)) {
			width.i <- max(sapply(str_trim(y[i,]), nchar))
			#	remove blacks!
			tmp.i <- format(str_trim(y[i,]),
				width = width.i, justify = "right")
			m.j <- matrix(" ",
				nrow = width.i + ifelse(add.lines, 1, 0),
				ncol = length(tmp.i))
			for (j in 1:width.i) {
				m.j[j,] <- sapply(tmp.i, function (x) substring(x, j, j))
			}
			m[[i]] <- cbind(rep(labels[i], width.i + ifelse(add.lines, 1, 0)), m.j)
		}
		
		y <- do.call("rbind", m)
		#	remove duplicated labels
		y[duplicated(y[,1]), 1] <- ""
		
		#	insert layer column
		y <- cbind(
			format(y[, 1], width = nchar(taxon[1])), # variable names
			format("", width = nchar(layer[1])), # layer column
			y[, -1] # values
			)
		
		#	promote table column names
		tmp <- dimnames(x)[[2]]
		width <- max(sapply(tmp[-c(1,2)], nchar)) # omit the first and second column
		z <- matrix(" ",
			nrow = width, ifelse(add.lines, 1, 0),
			ncol = length(tmp))
		for (i in 1:width) {
			xx <- substring(tmp, i, i)
			if (any(xx == "")) xx[xx == ""] <-  " "
			z[i,] <- xx				
		}
		
		#	clean first two columns
		z[, 1] <- format("", width = nchar(taxon[1]))
		z[, 2] <- format("", width = nchar(layer[1]))
		#	add label
		z[1,1] <- format("plot", width = nchar(taxon[1]))
		
		#	combine parts
		res <- rbind(z, y, x)
		
		#	add vertical rule
		if (isTRUE(rule)) {
			#	rule.ind
			newcol <- rule.col + (ncol(res) - nrow(obj)) # left most colums
			res <- res[, sort(c(1:ncol(res), newcol))]
			res[, newcol + 1:getK(obj)] <- "|"	
		}
		
		#	paste columns to lines
		res <- cbind(
			as.vector(apply(res[, c(1,2)], 1,
				function (x) paste0(x, collapse = ""))),
			as.vector(apply(res[, -c(1,2)], 1,
				function (x) paste0(x, collapse = sep))))		
		res <- apply(res, 1, function (x) paste0(x, collapse = ""))
		
		#	add keywords
		zy <- 1:(nrow(z) + nrow(y))
		x <- (max(zy) + 1):(nrow(x) + max(zy))
		
		#	warp around latex environment
		if (latex.input) {
			res <- c("\\begin{verbatim}",
				"BEGIN HEAD", res[zy], "END HEAD",
				"BEGIN TABLE", res[x], "END TABLE",
				"\\end{verbatim}")
		}
		else {
			res <- c("BEGIN HEAD", res[zy], "END HEAD",
				"BEGIN TABLE", res[x], "END TABLE")
		}
	}
	if (!no.save) writeLines(res, file)
	
	return(invisible(res))
}

#if (!inGeneric("write.verbatim")) {
setGeneric("write.verbatim",
	function (obj, file, select, absence = ".", sep = " ", pad = 1, abbreviate = TRUE, short.names = FALSE, rule, add.lines = FALSE, latex.input = FALSE, table.nr = FALSE)
		standardGeneric("write.verbatim")
)
#}
setMethod("write.verbatim",
    signature(obj = "Vegsoup"),
	    .write.verbatimVegsoup
)

#	move this somewhere else!
setMethod("write.verbatim",
   signature(obj = "VegsoupPartition"),
		.write.verbatimVegsoup
)