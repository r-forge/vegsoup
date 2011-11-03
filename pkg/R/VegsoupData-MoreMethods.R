#	function to format cdata tag in kml
.placemark <- function (x, obj, website.url.path, thumbnail.url.path) {
	
plot <- unique(x[,1])
table <- x[, -1]	
 
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

website.url.path <- "http://sabotag.hausdernatur.at/vegsoup/albums/"
website.url <- paste(website.url.path, plot, sep = "")

thumbnail.url.path <- "http://sabotag.hausdernatur.at/vegsoup/thumbnails/"
thumbnail.url <- paste(thumbnail.url.path, plot, sep = "")

img <- paste(
	"<a href=\"", website.url, "\"", ">",
	"<img src=\"", thumbnail.url, "\"", " width=\"400\" >",
	"</a>", sep = "")

coordinates <- coordinates(obj@sp.points[obj@sp.points$plot == plot,])

point <- c(
	"<styleUrl>#downArrowIcon</styleUrl>",
	"<Point>",
		paste("<coordinates>", coordinates[1], ",", coordinates[2], ",0", "</coordinates>", sep =""),
	"</Point>")
	


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

.KMLVegsoupData <- function (obj, path, thumbnail.url.path, website.url.path, ...) {

if (missing(path)) {
	path <- getwd()	
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
species.list <- SpeciesLong(obj)
species.list$taxon <- Taxonomy(obj)$taxon[match(species.list$abbr, Taxonomy(obj)$abbr)]
species.list <- species.list[, c(1,5,3,4)]
species.list <- split(species.list, species.list$plot)

placemark <- unlist(sapply(species.list, function (x) .placemark(x, obj)))

res <- 	c(
	begin.kml,
	placemark,
	end.kml)	

con <- file(paste(path, "/vegsoup.kml", sep = ""))
	writeLines(res, con)
close(con)

return(invisible(res))


}

setGeneric("KML",
	function (obj, ...)
		standardGeneric("KML")
)

setMethod("KML",
    signature(obj = "VegsoupData"),
    .KMLVegsoupData
)

