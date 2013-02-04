\name{stack}
\alias{stack.species}
\alias{stack.sites}
\alias{stack.coordinates}
\title{
Stack Species Matrix, Sites Data or Spatial Coordinates
}
\description{
All function reshape a data frame between \sQuote{wide format} with measurements in separate columns to \sQuote{long format} with the measurements in separate rows.

Function \code{stack.species} converts a matrix like input object or file into a representation in long format, where each observation on a species consits of one row in a resulting data frame that has as many rows as there are single species occurences in the data set. In a similar fashion, function \code{stack.sites} can be used to derive a plot by variable matrix in long format, with columns 'plot', 'variable' and 'value'. Function \code{stack.readOGR} collects spatial coordinates from spatial data stored on disk by calls to \code{\link{readOGR}}. It then reshapes two or three (3d features) columns of spatial coordinates together with a plot identifier to a format as returned by \code{stack.sites}. Results of \code{stack.sites} and \code{stack.readOGR}  can be combined using \code{rbind} to form a complete sites data frame as demanded for \code{Vegsoup} objects. Results from \code{stack.species} do also conform to the definition of this class. 
%Extract coordinates from OGR data source
}
\usage{
stack.species(x, file, csv2 = TRUE, schema = c("abbr", "layer", "comment"),
              absence, verbose = FALSE)

stack.sites(x, file, csv2 = TRUE, schema = "plot", verbose = FALSE)

stack.coordinates(dsn, layer, schema, round = TRUE, verbose = TRUE)               
}

\arguments{
## Arguments to functions
\item{x}{ An object converted to a data frame. }
\item{file}{ Path to a csv-file}
\item{csv2}{ Use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}. }
\item{schema}{ Names of columns to be searched for. See \sQuote{Details}. }
\item{absence}{ Character used to code absences. Can be missing, see \sQuote{Details}. }
\item{verbose}{ If \code{TRUE} prints some diagnostic messages. }

## Arguments for function stack.readOGR
\item{dsn}{ Data source name. See \code{\link{readOGR}}. }
\item{layer}{ Layer name. See \code{readOGR} }
\item{round}{ Round decimals to given precision. See \sQuote{Details}. }
\item{\dots}{ additional arguments passed to \code{readOGR}. }
}
\details{

For \code{stack.species} the supported data frame, either read from file, or passed as \code{R} object must have columns corresponding to argument schema. The default schema is 'abbr', 'layer' and 'comment' (see class \code{\link{class?Species}}). The column 'comment' can have \code{NA} or \code{NULL}. This field can be used to store taxonomic or other relvant information (accuracy of determination) on a particular observation, but is not mandatory. The species matrix is assumed to have species in rows and plots in columns. Plot names are derived form \code{names(x)}. If argument file is supplied, all columns are imported using \code{read.csv(..., colClasses = "character")}. This matters if species abundances were recorded on a non-ordinal scale and schoud be kept as numeric. Internal checking of conversion is performed using \code{\link{type.convert}} to test for appropriate mode of the species abundances vector; setting \code{verbose = TRUE} will report results of this test. If argument \code{absences} is missing an almost save guess is obtained from the data and derived as the most frequent value in the species data matrix. These are usually the species absences.

For \code{stack.sites} theres are no specific requirements about the form of the object, except that one column must be specified which will then be used as the plot identifier. The default is to search for a column named \code{'plot'}, but an other column name can be specified by argument \code{schema}. 

For \code{stack.readOGR} the first element of schema defines the name of the column in the OGR data source that identifies unique plots. This argument is mandatory and must match a column name in the spatial attributes data. A second element of argument \code{schema} can specify the name of the column in the OGR data source that identifies altitude measurements. If absent and the geometry is 3D the function read the third (z) dimension if the data source supports it. If \code{verbose} the function will print what is returned by \code{\link{ogrInfo}}. Irrespective of the geometry type the function will use the \code{\link[sp:coordinates]{coordinates}} method in package \pkg{sp} to obtain a pair of geographic coordinates for each plot. In case of irregular polygons the centroid so obtained must not be accurate. If the coordinte reference system is not equivalent to \code{CRS("+init=epsg:4326")} it will be transformed.
%Using WGS decimal coordinates, 6 decimals places are sufficient for a precision at the meter level
%If some values are missing those will not raise NA's but will set to zero elevation.

}

\value{
\code{stack.species} returns an object of class \code{"Species"} with slot \code{data}: a five-column data frame; the first column has the plot names (\code{'plot'}), the second species abbreviations (\code{'abbr'}), the third the respective layer (\code{'layer'}), the fourth the abundance values (\code{'cov'}) and the last columns represents a comment field (\code{'comment'}).
\cr
\code{stack.sites} returns an object of class \code{"Sites"} with slot \code{data}: a three-column data frame; the first column has the plot names (\code{'plot'}), the second the measured variable (\code{'variable'}), and the last the respective value (\code{'value'}) for each variable measured on each plot. Colomn 'value' is coerced to \code{as.character}.
}

\author{
Roland Kaiser (\email{kardinal.eros@gmail.com})
}

\examples{
library(vegan)
data(dune)

## species matrix
#  there are two moss species in the dune data set
x <- data.frame(abbr = names(dune),
	layer = c(rep("hl", 8), "ml", rep("hl", 6), "ml", rep("hl", 14)),
	comment = "", t(dune))
#	groom plot names
names(x)[4:ncol(x)] <- gsub("X", "dn", names(x)[4:ncol(x)])

spc <- stack.species(x, verbose = TRUE)

## sites data frame
data(dune.env)
x <- data.frame(plot = row.names(dune.env), dune.env)
#	groome plot names
x$plot <- paste("dn", x$plot, sep = "")


sts <- stack.sites(x, verbose = TRUE)

}

\seealso{
\code{\link{Species-class}},
\code{\link{Sites-class}},
\code{\link{reshape.species}},
\code{\link{read.csv}},
\code{\link{readOGR}} in package \pkg{rgdal}
}

\keyword{import}

