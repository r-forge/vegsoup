\name{stack}

\alias{stackSpecies}
\alias{stackSites}
\alias{stackCoordinates}

\title{ Stack Species Matrix, Sites Data or Spatial Coordinates }

\description{
  All functions reshape a data frame between \emph{wide format} with
  measurements in separate columns to \emph{long format} with the measurements
  in separate rows (see \code{link{reshape}}).

  The \emph{long format} is called as such, because more sampling results in a
  lengthening of the data (i.e., the addition of rows) without any widening
  (i.e., the addition of columns). In contrast, it is common in community
  ecology to store abundance data as spreadsheets with sites as
  rows and species as columns. Such a data storage format is often called
  \emph{wide format}, because more sampling may result in a widening of the
  data (e.g., more columns are required as further sampling reveals an increasing
  number of species). 

  Function \code{stackSpecies} converts a matrix like input object or file
  into a representation in long format, where each observation on a species
  consists of one row in a resulting data frame that has as many rows as there
  are single species occurrences in the data set (see
  \code{\linkS4class{Species}}). In a similar fashion, function
  \code{stackSites} can be used to derive a plot by variable matrix in long
  format (see \code{\linkS4class{Sites}}). Function \code{stackCoordinates}
  collects spatial coordinates from spatial data stored on disk by calls to
  \code{\link[rgdal]{readOGR}}. It then reshapes two or three (3D features) columns
  of spatial coordinates together with a plot identifier to the format defined
  by class \code{"Sites"}. Results of \code{stackSites} and
  \code{stackCoordinates} can be combined using \code{rbind} to form a complete
  sites data frame as demanded for \code{Vegsoup} objects.

}

\usage{
stackSpecies(x, file, sep = ";", dec = ",", schema = c("abbr", "layer", "comment"),
              absences, verbose = FALSE)

stackSites(x, file, sep = ";", dec = ",", schema = "plot", verbose = FALSE)

stackCoordinates(dsn, layer, schema, round = TRUE, verbose = TRUE, ...)               
}

\arguments{
## Arguments to functions
  \item{ x }{
     An object converted to a data frame.
  }
  \item{ file }{
     Path to a csv-file
  }
  \item{ sep, dec }{
    See \code{\link{read.csv}}.
  }
  \item{ schema }{
     Names of columns to be searched for. See \sQuote{Details}.
  }
  \item{ verbose }{
     Prints some diagnostic messages.
  }
## Arguments for function stackSpecies
  \item{ absences }{
    Character used to code absences. Can be missing, see \sQuote{Details}.
  }

## Arguments for function stackCoordinates
  \item{ dsn }{
    Data source name. See \code{\link[rgdal]{readOGR}}.
  }
  \item{ layer }{
    Layer name. See \code{readOGR}.
  }
  \item{ round }{
    Round decimals to given precision. See \sQuote{Details}.
  }
  \item{ \dots }{
    additional arguments passed to \code{readOGR}.
  }
}

\details{
For \code{stackSpecies} the supported data frame, either read from file, or passed as \code{R} object must have columns corresponding to argument schema. The default schema is 'abbr', 'layer' and 'comment' (see class \code{\linkS4class{Species}}). The column 'comment' can have \code{NA} or \code{NULL}. This field can be used to store taxonomic or other relvant information (accuracy of determination) on a particular observation, but is not mandatory. The species matrix is assumed to have species in rows and plots in columns. Plot names are derived form \code{names(x)}. If argument file is supplied, all columns are imported using \code{read.csv(..., colClasses = "character")}. This matters if species abundances were recorded on a non-ordinal scale and schoud be kept as numeric. Internal checking of conversion is performed using \code{\link{type.convert}} to test for appropriate mode of the species abundances vector; setting \code{verbose = TRUE} will report results of this test. If argument \code{absences} is missing an almost save guess is obtained from the data and derived as the most frequent value in the species data matrix. These are usually the species absences.

For \code{stackSites} theres are no specific requirements about the form of the object, except that one column must be specified which will then be used as the plot identifier. The default is to search for a column named \code{'plot'}, but an other column name can be specified by argument \code{schema}. 

For \code{stackCoordinates} the first element of schema defines the name of the column in the OGR data source that identifies unique plots. This argument is mandatory and must match a column name in the spatial attributes data. A second element of argument \code{schema} can specify the name of the column in the OGR data source that identifies altitude measurements. If absent and the geometry is 3D the function read the third (z) dimension if the data source supports it. If \code{verbose} the function will print what is returned by \code{\link[rgdal]{ogrInfo}}. Irrespective of the geometry type the function will use the \code{\link[sp:coordinates]{coordinates}} method in package \pkg{sp} to obtain a pair of geographic coordinates for each plot. In case of irregular polygons the centroid so obtained must not be accurate. If the coordinte reference system is not equivalent to \code{CRS("+init=epsg:4326")} it will be transformed.
%Using WGS decimal coordinates, 6 decimals places are sufficient for a precision at the meter level
%If some values are missing those will not raise NA's but will set to zero elevation.
}

\value{
\code{stackSpecies} returns an object of class \code{\linkS4class{Species}} \cr
\code{stackSites} and \code{stackCoordinates} returns an object of class \code{\linkS4class{Sites}}.
}

\author{ Roland Kaiser }

\examples{
require(vegsoup)
require(vegan)
data(dune)

## species matrix
#  there are two moss species in the dune data set
x <- data.frame(abbr = names(dune),
                layer = c(rep("hl", 8), "ml",
                          rep("hl", 6), "ml",
                          rep("hl", 14)),
                comment = "", t(dune))
# groom plot names
names(x)[4:ncol(x)] <- gsub("X", "dn", names(x)[4:ncol(x)])

spc <- stackSpecies(x, verbose = TRUE)

## sites data frame
data(dune.env)
x <- data.frame(plot = row.names(dune.env), dune.env)
#	groome plot names
x$plot <- paste("dn", x$plot, sep = "")

sts <- stackSites(x, verbose = TRUE)

## dummy coordiantes example
dsn <- tempfile()
layer <- "foo"

#	spatial points
pts <- data.frame(x = rnorm(20), y = rnorm(20),
                  plot = unique(spc$plot))
coordinates(pts) <- ~x+y
proj4string(pts) <- CRS("+init=epsg:4326") # WGS84

dsn <- tempfile()
layer <- "foo"

writeOGR(pts, dsn, layer, driver = "ESRI Shapefile")

sts.xy <- stackCoordinates(dsn, layer, schema = "plot")

sts <- rbind(sts, sts.xy)
}

\seealso{
\code{\link{Species-class}},
\code{\link{Sites-class}},
\code{\link{reshape.species}},
\code{\link{elevation}},
\code{\link{tile2latlng}},
\code{\link{read.csv}},
\code{\link[rgdal]{readOGR}} in package \pkg{rgdal}
}

\keyword{ import }
