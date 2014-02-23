\name{tile2latlng}

\alias{latlng2tile}
\alias{tile2latlng}
\alias{str2latlng}
\alias{whatshere2lnglat}

\title{ Geocoding Using Tile Map Service Protocol }
\description{
There are several ways to obtain coordinates for biological observations. This set of functions provides quick and dirty geocoding functionality using the grid scheme of tile map services such as google map tiles or OpenStreetMap.
}
\usage{
latlng2tile(lat, lng, z)
tile2latlng(x, y, z, offset = 0)
str2latlng(xyz, offset = 0.5, sep = ",", style = c("google", "osm"))
}
\arguments{
  \item{ lat }{
A numeric vector of length 1.
}
  \item{ lng }{
A numeric vector of length 1.
}
  \item{ z }{
Zoom level of tile (see arguments \code{x} and \code{y})
}
  \item{ x }{
Row (x index) tile number in given zoom level (see argument \code{z}).
}
  \item{ y }{
Column (y index) tile number in given zoom level.
}
  \item{ xyz }{
A character vector giving the relevant part of an URL request to obtain a tile. See \sQuote{Details} on how to obtain a single centroid coordinate for a series of tiles.
}
  \item{ offset }{
A Numeric of length 1 and in the range of 0 and 1. See \sQuote{Details}.
}
  \item{ sep }{
A character serving as sperator if \code{xyz} is a single string composed of more than one snippets of URL requests. See \sQuote{Details}.
}
  \item{ style }{
URL protocol style. Currently only google maps API (\code{'google'}) or OpenStreetMap API (\code{'OSM'}).
}
}
\details{
All current tile server expect tiles to be served up at URLs following a specific schema and any software that request such tiles selects the relevant ones for display to be within a spatial bounding box. It follows that the unique specifier of a particular tile has intrinsic information of the spatial extent of that tile.

The idea about the here provided functions is to use the world wide unique tile specifier used by popular map providers (google maps, OpenStreetMap) and direct interfaces to their services, to obtain coordinates for objects that need to be geo-referenced. Such needs arise when herbarium vouchers or vegetation plots are referenced. Often the textual description of a location is vague and coordinates can only be determined with uncertainty. Reporting such uncertainties is essential and should always be done. Nevertheless this requires an estimate of uncertainty which by itself can't be determined objectively and is associated with additional labour (measuring tool).

Using a web development tool such as firebug it is intuitive and convenient to inspect all the URLs (each tile equals a single request to be sent to the tile server) that are constructed when map scene is displayed on screen. From the development tool console each URL request for each tile can be highlighted by moving the mouse pointer over the map (click firebug's inspect button). It then needs just copy and paste of the relevant part of the url specifying the highlighted tile. The relevant part looks like \code{"x=35144&s=&y=22834&z=16"} and may have an additional argument \code{&s=} which is usually empty. OpenStreetMap encodes the same tile as \code{"src="http://a.tile.openstreetmap.org/16/35144/22834.png""} where the essential part is \code{"16/35144/22834"}. Getting back to the discussion about coordinate uncertainty, it is convenient to use the zoom level to obtain a surrogate of coordinate precision. This is straight forward, as each zoom level is associated with a specific scale following a power law.

Function \code{str2latlng} accepts parts of an URL as outlined above and returns the corner or center coordinates of the specified tile (see argument \code{offset} and below). If argument \code{xyz} contains more than one URL request of the protocol exemplified above in a single character element (a series of copy and paste), and each separated with a user defined special character (see argument \code{sep}), the function will return the centroid coordinates of all tiles involved. The same applies if \code{xyz} is a character vector of length longer than 1, where each element is a valid single argument to \code{xyz}.

Function \code{tile2latlng} provides an interface where \code{x}, \code{y} and \code{z} can be specified separately and \code{latlng2tile} performs the reverse operation.

Argument \code{offset} of functions \code{tile2latlng} and \code{str2latlng} can be used to obtain upper left (\code{offset = 0}), center (\code{offset = 0.5}) or lower right (\code{offset = 1}) corner coordinates of the tile.
}
\value{
A numeric vector having latitude and longitude at first, respectively, second position. \code{str2latlng} additionally reports precision (half the diagonal of the tile) and zoom level as a surrogate of coordinate uncertainty.
}
\references{

See the open street map wiki for all details about the tile protocols.

\url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{read.verbatim}},
\code{\link{elevation}}
}
\examples{
# a unique tile given coordiantes and zoom level 
( xyz <- latlng2tile(47.00000, 13.00000, 14) )

# paste string as it would display in the web development console
( xyz <- paste0("x=", xyz[1], "&y=", xyz[2], "&z=", xyz[3]) )

# center coordinates of tile
str2latlng(xyz, offset = 0.5)

# centroid of two tiles, sep is ','
xyz <- "x=17547&s=&y=11516&z=15,x=17548&s=&y=11516&z=15"

# center coordinates of two tiles
str2latlng(xyz, offset = 0.5)

# upper left coordinate
tile2latlng(17547, 11516, 15, offset = 0)

}
\keyword{ manip }

