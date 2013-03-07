\name{Vegsoup}
\alias{Vegsoup}
\alias{instantsoup}

\title{ Create an object of class vegsoup }
\description{
  The provided creator functions build the basic data structures of
  \code{Vegsoup*} objects from local R objects or from csv files stored on
  disk. A shorthand function is provided to collect data files from a folder.
}
\usage{
Vegsoup(x, y, z, coverscale, decostand, dist, group, sp.points, sp.polygons,
        proj4string = "+init=epsg:4326", verbose = FALSE)
instantsoup(folder, sep = ";", dec = ",", coverscale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ x }{
    Object of class \code{\linkS4class{Species}} or
    \code{\linkS4class{SpeciesTaxonomy}}.
  }
  \item{ y }{
    Object of class \code{\linkS4class{Sites}}.
  }
  \item{ z }{
    Object of class \code{\linkS4class{Taxonomy}}. Can be missing if \code{x}
    is of class \code{"SpeciesTaxonomy"}.
  }
  \item{ folder }{
    Path to a folder with three csv files. One file named \code{species.csv}
    represents a table that can be coerced to class \code{"Species"} by
    simply calling \code{species("./species.csv")}, the other one gives sites
    (\code{sites("./sites.csv")}) and the third one a taxonomic reference
    list  (\code{taxonomy("./sites.csv")}). If files for species and or sites
    have the file suffix \emph{wide}, functions \code{stack.species} and
   \code{stack.sites} are called.
  }
  \item{ coverscale }{
    Abundance scale of the observations. Possible choices include all
    definitions given by \code{\link{Coverscale}}.
  }
  \item{ group }{
    A-priori grouping, if any and can be missing.
  }
  \item{ proj4string }{
    Projection attributes for slots \code{sp.points} and \code{sp.polygons}.
    If missing set to \code{CRS("+init=epsg4326")}
    (see \code{\link{proj4string}} for details).
  }
  \item{ sp.points }{
    Object of class \code{\link{SpatialPointsDataFrame}}, if missing
    coordinates are seeked in argument \code{'y'}. If coordinate information
    can not be extracted, a random point pattern in the unit square is
    generated.
  }
  \item{ sp.polygons }{
    Object of class \code{\link{SpatialPolygonsDataFrame}}, if missing
    a random polygon pattern is generated based on \code{'sp.points'} equal to
    the number of plots (samples).
  }
  \item{ sep }{
    See \code{\link{read.csv}} for parsing of csv files. Defaults are set
    according to \code{read.csv2}, also regarding \code{dec}!
  }
  \item{ dec }{
    See \code{read.csv}. 
  }
  \item{ verbose }{
    Show status reports during function call.
  }
}
\details{
  \code{Vegsoup()} is the generic constructor function to create a valid
  object of class \code{Vegsoup}. If argument \code{'x'} inherits from class
  "SpeciesTaxonomy", argument \code{'z'} can be missing.

  If the sites data has variables exactly named longitude and latitude those
  will be used to extract coordinates. For coordinates conforming to
  \code{proj4string = "+init=epsg:4326"} those have to be formatted as decimal
  degrees! See e.g. \code{\link{char2dms}} to convert between formats.

%%\code{Vegsoup} will discard any alphanumeric characters by calling
%%\code{gsub(pattern = "[[:alpha:]]", replacement = "", ...}. Decimals (","
%%and ".") are handled to yield numeric values for columns that can be coerced
%%to \code{as.numeric} (e.\,g. in x\$cov if abundance is not ordinal, or for
%%all in y that represent continuous measurement on plot.).

  \code{instantsoup} is a shorthand when raw data files are guaranteed to
  conform to some format that can be interpreted by the import functions
  \code{stack.species}, respectively \code{stack.sites} or are valid to simple
  calls to class creator methods \code{\link{species}}, \code{\link{sites}}
  and \code{\link{taxonomy}} for signature \code{'character'}. Arguments
  \code{'coverscale'} and \code{'decostand'} can be specified afterwards
  if need, defaults of class \code{\linkS4class{Vegsoup}} apply otherwise. The
  same is true for argument \code{'proj4string'}.
}
\note{
  If you gain errors using \code{instantsoup}, try to create separate objects
  for classes \code{"Species"}, \code{"Sites"} and \code{"Taxonomy"}. You may
  also want to call \code{"SpeciesTaxonomy"} for the first and last objects
  mentioned. Defaults for \code{sep} and \code{dec} passed to \code{read.csv}
  are set according to \code{read.csv2}!
}
\section{Warning}{
All missing values that might result when casting \code{y} will be replaced with zeros, not \code{NA}!

If sp.points and/or sp.polygons are supplied the data slot (\code{SpatialPointsDataFrame@data}) must have a column named plot. In addition, this column has to be ordered. Sorting can be performed using \code{SpatialPointsDataFrame[order(SpatialPointsDataFrame\$plot), ]}. The same is mandatory of slot \code{sp.polygons}.

When suppling argument \code{proj4string} please ensure that \code{CRS(proj4string)} works.
}
\value{
An object of class Vegsoup, see class \code{\linkS4class{Vegsoup}}.
}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
library(vegsoup)
data(testdata)

qry <- Vegsoup(spc, sts, txa,
               coverscale = "braun.Blanquet")
qry

	
}
\seealso{
\code{\link{Vegsoup-class}}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
