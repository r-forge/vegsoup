.KMLVegsoup <- function (obj, file, thumbnail.url.path, website.url.path, ...) {

#	function to format cdata tag in kml
.placemark <- function (x, obj, website.url.path, thumbnail.url.path) {
	
plot <- unique(x[,1])
table <- x[, -1]	

if (missing(website.url.path)) {
	website.url.path <-
		"http://sabotag.hausdernatur.at/vegsoup/albums/"
}
if (missing(thumbnail.url.path)) {
	thumbnail.url.path <-
		"http://sabotag.hausdernatur.at/vegsoup/thumbnails/"
}
		
website.url <- paste(website.url.path, plot, sep = "")
thumbnail.url <- paste(thumbnail.url.path, plot, sep = "")

begin.placemark <- c(
	"<Placemark>",
	"<name>", plot, "</name>")

end.placemark <- c(
	"</Placemark>"
)

begin.description <- c(
	"<description>",
 		"<![CDATA[",
			"<table border=\"0\" >"
)
end.description <- c(
		"]]>",
	"</description>"
)					

img <- paste(
	"<a href=\"", website.url, "\"", ">",
	"<img title=\"Klick for Gallery\" src=\"", thumbnail.url, "\"", " width=\"400\" >",
	"</a>", sep = "")

coordinates <- coordinates(obj@sp.points[obj@sp.points$plot == plot,])

point <- c(
	"<styleUrl>#downArrowIcon</styleUrl>",
	"<Point>",
		paste("<coordinates>", coordinates[1], ",", coordinates[2], ",0", "</coordinates>", sep =""),
	"</Point>")


#<th style="border-bottom: 1px solid #000000;">taxon</th><th style="border-bottom: 1px solid #000000;">layer</th><th style="border-bottom: 1px solid #000000;">cov</th>

begin.table <- 
	c("<tr>",
	paste("<th>", names(table)[1], "</th><th>", names(table)[2], "</th><th>", names(table)[3], "</th>", sep = ""),
	"</tr>")
	
table.body <- c(apply(table, 1,
	FUN = function (x) {
		c(
			"<tr>",
			"<td><i>", x[1], "</i></td><td>", x[2], "</td><td>", x[3], "</td>",
			"</tr>"
		)
	}
	))
	
end.table <- c(
	"</table>",
	"<p>"
)	

res <- c(
	begin.placemark,
	begin.description,
	begin.table,
	table.body,
	end.table,
	img,
	end.description,
	point,
	end.placemark
	)
	
res	
}

if (missing(file)) {
	file <- paste(getwd(), "/vegsoup.kml", sep = "")
	warning("\nargument file missing, drop KML to folder ",
		getwd(), " as ./vegsoup.kml")
}	
begin.kml <- c(
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
"<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">",
	"<Document>",
	"<name>vegsoup plots</name>",
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
		
#obj = dta
species.list <- Species(obj)
species.list$taxon <- Taxonomy(obj)$taxon[match(species.list$abbr, Taxonomy(obj)$abbr)]
species.list <- species.list[, c(1,5,3,4)]
species.list <- split(species.list, species.list$plot)

placemark <- unlist(sapply(species.list, function (x) .placemark(x, obj)))

res <- 	c(
	begin.kml,
	placemark,
	end.kml)	

con <- file(file)
	writeLines(res, con)
close(con)

return(invisible(res))
}

setGeneric("KML",
	function (obj, ...)
		standardGeneric("KML")
)

setMethod("KML",
    signature(obj = "Vegsoup"),
    .KMLVegsoup
)

#	KML output
.KMLVegsoupPartition <- function (obj, path, add.label = FALSE, website.url.path, thumbnail.url.path, ...) {
if (missing(path)) {
	path <- getwd()	
}
if (missing(website.url.path)) {
	website.url.path <-
		"http://sabotag.hausdernatur.at/vegsoup/albums/"
}
if (missing(thumbnail.url.path)) {
	thumbnail.url.path <-
		"http://sabotag.hausdernatur.at/vegsoup/thumbnails/"
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
	ifelse(add.label, paste("<name>", x[4], "</name>", sep = ""), ""),
	paste("<styleUrl>#partition", x[1], "</styleUrl>", sep = ""),
	"<description>",
	paste("plot", x[4], "partition", x[1]),
	#	add gallery
	paste(
		"<![CDATA[<a href=\"", paste(website.url.path, x[4], sep = ""), "\"", ">",
		"<img title=\"Klick for Gallery\" src=\"",
		paste(thumbnail.url.path, x[4], sep = ""), "\"", " width=\"400\" >",
		"</a>]]>", sep = ""),	
	"</description>",
	"<ExtendedData>",
	paste("partition", x[1]),
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
	coordinates(obj), plot = names(Partitioning(obj)), stringsAsFactors = FALSE)

points$website.url <- paste(website.url.path, points$plot, sep = "")
points$thumbnail.url <- paste(thumbnail.url.path, points$plot, sep = "")

folder <- unlist(sapply(unique(points$partitioning), FUN = function (x) {
	res <- c(
	"<Folder>",
		paste("<name>partition", x, "</name>"),
		c(apply(points[points$partitioning == x, ], 1, .placemark)),
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

#	x = 1
#	c(apply(points[points$partitioning == x, ], 1, .placemark))	
con <- file(paste(path, "/vegsoup partition.kml", sep = ""))
	writeLines(res, con)
close(con)
	
return(invisible(res))
}

setMethod("KML",
   signature(obj = "VegsoupPartition"),
    .KMLVegsoupPartition
)