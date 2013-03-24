\name{Vegsoup}
\alias{Vegsoup}
\alias{instantsoup}
\alias{coenoflex}

\title{ Create an object of class \code{"Vegsoup"} }
\description{
  The provided creator functions build the basic data structures of
  \code{Vegsoup*} objects from local R objects or from csv files stored on
  disk. A shorthand function is provided to collect data files from a folder.

  \code{Vegsoup} is the recommended user interface rather than direct
  \code{link{new}}\code{("Vegsoup", ....)} call.
}
\usage{
Vegsoup(x, y, z, coverscale, group, sp.points, sp.polygons,
        proj4string = "+init=epsg:4326", verbose = FALSE)

instantsoup(folder, sep = ";", dec = ",", coverscale)

coenoflex(numplt = 10, numspc = 10, ...)
}

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
    as \code{sites("./sites.csv")} and the third one a taxonomic reference
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
  \item{ numplt }{
    See \code{\link[coenoflex:coenoflex]{coenoflex}}.
  }
  \item{ numspc }{
    See \code{"coenoflex"}
  }
  \item{ \dots }{
    Additional arguments passed to coenoflex.
  }
  \item{ verbose }{
    Show status reports during function call.
  }
}
\details{
  \code{Vegsoup()} is the generic constructor function to create a prototype
  object of class \code{Vegsoup}. The first four arguments
  (\code{x, y, z, coverscale}) have to be specified. However, if argument
  \code{'x'} inherits from class "SpeciesTaxonomy", argument \code{'z'} can be
  left missing.
  
  The creator function does not allow to specify all slots of an
  \code{Vegsoup*} object, but rather tries to guess some of it's contents.
  For example, if the sites data has variables exactly named longitude and
  latitude those will be used to extract coordinates. For coordinates
  conforming to \code{proj4string = "+init=epsg:4326"} those have to be
  formatted as decimal degrees! See e.g. \code{\link{char2dms}} to convert
  between formats.

%%\code{Vegsoup} will discard any alphanumeric characters by calling
%%\code{gsub(pattern = "[[:alpha:]]", replacement = "", ...}. Decimals (","
%%and ".") are handled to yield numeric values for columns that can be coerced
%%to \code{as.numeric} (e.\,g. in x\$cov if abundance is not ordinal, or for
%%all in y that represent continuous measurement on plot.).

  Function \code{instantsoup} is a shorthand when data files are guaranteed to
  conform to some format that can be interpreted by the import functions
  \code{stack.species}, respectively \code{stack.sites} or are valid to simple
  calls to class creator methods \code{\link{species}}, \code{\link{sites}}
  and \code{\link{taxonomy}} for signature \code{'character'}. Arguments
  \code{'coverscale'} and \code{'decostand'} can be specified afterwards
  if needed, defaults of class \code{\linkS4class{Vegsoup}} apply otherwise.
  The same is true for argument \code{'proj4string'}. The defaults for
  argument \code{'group'} is to create a dummy integer variable of ones.

  Function \code{coenflex} creates a random community.
}
\note{
  If you gain errors using \code{instantsoup}, try to create separate objects
  for classes \code{"Species"}, \code{"Sites"} and \code{"Taxonomy"}. You may
  also want to call \code{"SpeciesTaxonomy"} for the first and last objects
  mentioned. Defaults for \code{sep} and \code{dec} passed to \code{read.csv}
  are set according to \code{read.csv2}!
}
\section{Warning}{
  All missing values that might result when casting \code{y} will be replaced
  with zeros, not \code{NA}!

  If sp.points and/or sp.polygons are supplied the data slot
  (\code{SpatialPointsDataFrame@data}) must have a column named plot. The same
  is mandatory of slot \code{sp.polygons}.

  When suppling argument \code{proj4string} ensure that
 \code{CRS(proj4string)} works.
}
\value{
An object of class \code{\linkS4class{Vegsoup}}.
}
\author{ Roland Kaiser }
\examples{
library(vegsoup)

# real data
data(testdata)

dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")
dta

# plot ordinationsimulated data
plot(metaMDS(as.numeric(coenoflex(100, 100))))

# promote to ordinal scale
coverscale(sim) <- "braun.blanquet"
sim
}
\seealso{
\code{\link{Vegsoup-class}}

}
\keyword{ classes }
