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

#	add plots to parttion column
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
	i.part <- i.part[,order(DecomposeNames(i.part)$layer, decreasing = TRUE)]
	
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
	writeLines(paste("\\input{",
			gsub(path, "", filenames, fixed = TRUE),
			"}", sep = ""), con)
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

#	kml output
.KMLVegsoupDataPartition <- function (obj, path, ...) {
if (missing(path)) {
	path <- getwd()	
}
#	obj = prt

#	to do!
#	implement roll over labels.


begin.kml <- c(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
"<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">",
	"<Document>",
	"<name>vegsoup partitions</name>",
	"<Style id=\"downArrowIcon\">",
		"<IconStyle>",
        	"<Icon>",
          		"<href>http://maps.google.com/mapfiles/kml/pal4/icon28.png</href>",
        	"</Icon>",
		"</IconStyle>",
    "</Style>")
end.kml <- c(
	"</Document>",
	"</kml>")
	
.style.numbers.normal <- function (x) {
	c(paste("<Style id=\"partition_normal", x, "\">", sep = ""),
		"<IconStyle>",
			"<scale>1.1</scale>",
			"<Icon>",
				paste("<href>http://maps.google.com/mapfiles/kml/paddle/", x, ".png</href>", sep = ""),
			"</Icon>",
			"<hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/>",
		"</IconStyle>",
		"<LabelStyle>",
		"</LabelStyle>",
		"<BalloonStyle>",
			"<text>$[description]</text>",
		"</BalloonStyle>",
	"</Style>")
}

.style.numbers.highlight <- function (x) {
	c(paste("<Style id=\"partition_highlight", x, "\">", sep = ""),
		"<IconStyle>",
			"<scale>1.2</scale>",
			"<Icon>",
				paste("<href>http://maps.google.com/mapfiles/kml/paddle/", x, ".png</href>", sep = ""),
			"</Icon>",
			"<hotSpot x=\"32\" y=\"1\" xunits=\"pixels\" yunits=\"pixels\"/>",
		"</IconStyle>",
		"<LabelStyle>",
			"<scale>1.2</scale>",
		"</LabelStyle>",
		"<BalloonStyle>",
			"<text>$[description]</text>",
		"</BalloonStyle>",
	"</Style>")
}

.stylemap.numbers <- function (x) {
		c(
		paste("<StyleMap id=\"partition", x, "\">", sep = ""),
		"<Pair>",
			"<key>normal</key>",
			paste("<styleUrl>#partition_normal", x, "</styleUrl>", sep = ""),
		"</Pair>",
		"<Pair>",
			"<key>highlight</key>",
			paste("<styleUrl>#partition_highlight", x, "</styleUrl>", sep = ""),
		"</Pair>",
		"</StyleMap>")	
}
.placemark <- function (x) {
	c(
	"<Placemark>",
	paste("<name>", x[4], "</name>", sep = ""),
	paste("<styleUrl>#partition", x[1], "</styleUrl>", sep = ""),
	"<description>",
	paste("partition", x[1]),
	"</description>",
	"<ExtendedData>",
	paste("plot", x[4]),
	"</ExtendedData>", 
#	"<gx:balloonVisibility>0</gx:balloonVisibility>",
	"<Point>",
		paste("<coordinates>", x[2], ",", x[3], ",0", "</coordinates>", sep =""),
	"</Point>",
	"</Placemark>")
}	
		
#obj = prt

if (max(Partitioning(obj)) > 10) {
	if (max(Partitioning(obj)) < 26) {
		warning("numbered styled KML ouput is currently limited to 10 groups",
			"\nuse alphabet as alternative to numbers")
		paddle.file <- LETTERS[unique(Partitioning(obj))]
		paddle.identifier <- LETTERS[Partitioning(obj)]
		Partitioning(obj)
	} else {
		stop("styled KML ouput is currently limited to 26 groups (letter coding)")
	}
	} else {	
		paddle.file <- unique(Partitioning(obj))
		paddle.identifier <- Partitioning(obj)
	}

styles.normal <- c(sapply(paddle.file, .style.numbers.normal))
styles.highlight <- c(sapply(paddle.file, .style.numbers.highlight))
stylemap <- c(sapply(paddle.file, .stylemap.numbers))

#	to do! order folders!

points <- data.frame(partitioning = paddle.identifier,
	coordinates(obj), plot = names(Partitioning(obj)))

folder <- unlist(sapply(unique(points$partitioning), FUN = function (x) {
	res <- c(
	"<Folder>",
		paste("<name>partition", x, "</name>"),
		c(apply(points[points$partitioning == x,], 1, .placemark)),	
	"</Folder>")
	}
))
res <- c(
	begin.kml,
	styles.normal,
	styles.highlight,
	stylemap,
	folder,
	end.kml)
	
con <- file(paste(path, "/vegsoup partiton.kml", sep = ""))
	writeLines(res, con)
close(con)
	
return(invisible(res))
}

setMethod("KML",
   signature(obj = "VegsoupDataPartition"),
    .KMLVegsoupDataPartition
)