\encoding{UTF-8}

\name{Vegsoup}

\alias{Vegsoup}

\alias{instantsoup}

\alias{plainsoup}

\alias{coenoflex}

\title{ Create an object of class Vegsoup }

\description{
  The provided creator functions build the basic data structures of
  \emph{Vegsoup*} objects from local R objects. Shorthand functions
  are provided to collect data files from a folder or build an object
  from a simple community matrix only, with dummy variables created
  for missing data structures (taxonomic reference list, sites data and/or
  coordinates). To simulate a data set an interface to the \emph{coenoflex}
  package is provided.

  \code{Vegsoup(...)} is the recommended user interface rather than the direct
  \code{new("Vegsoup", ....)} call.
}

\usage{
Vegsoup(x, y, z, coverscale, group, sp.points, sp.polygons,
        proj4string = "+init=epsg:4326", stringsAsFactors = TRUE,
        verbose = FALSE)

coenoflex(i = 30, j = 20, gradients = 2, ...)

plainsoup(m, coverscale, layer = "0l", verbose = TRUE)

instantsoup(folder, sep = ";", dec = ",", coverscale)
}

% package coenoflex is not available anymore

\arguments{
  \item{ x }{
    object of class \code{\linkS4class{Species}} or
    \code{\linkS4class{SpeciesTaxonomy}}.
  }
  \item{ y }{
    object of class \code{\linkS4class{Sites}}.
  }
  \item{ z }{
    object of class \code{\linkS4class{Taxonomy}}. Can be missing if \code{x}
    is of class \code{'SpeciesTaxonomy'}.
  }
  \item{ m }{
    object that can be coereced to as matrix with plots as rows and species as columns.
  }
  \item{ layer }{
    layer code to be assigned.
  }
  \item{ folder }{
    Path to a folder with three csv files. One file named \code{species.csv}
    represents a table structure that can be coerced to class \code{'Species'} by
    simply calling \code{\link{species}("species.csv")}. Another file is coerced
    by \code{\link{sites}("sites.csv")} and the third one, a taxonomic reference
    list, is loaded via calls to \code{\link{taxonomy}("./taxonomy.csv")}).
    For all these method the \code{character} dispatch is invoked
    (cp. \code{signature(obj = "character")} in the methods section for the
    respective method). If files for species and or sites
    carry the file suffix \emph{wide} seperated with a blank,
    functions \code{\link{stackSpecies}} and \code{\link{stackSites}} are called.
  }
  \item{ coverscale }{
    Abundance scale of the observations. Possible choices include all
    definitions given by \code{\link{Coverscale}}. For \code{plainsoup}
    currently only an object of class \code{Coverscale} is accepted.
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
  \item{ stringsAsFactors }{
    Convert character strings in \code{y} to factors.
  }
  \item{ sep }{
    See \code{\link{read.csv}} for parsing of csv files. Defaults are set
    according to \code{read.csv2}, also regarding \code{dec}!
  }
  \item{ dec }{
    See \code{read.csv}. 
  }
  \item{ i, j, gradients }{
    %See \code{\link[coenoflex:coenoflex]{coenoflex}}.
    number of plots (\code{n}) and species (\code{m}) to simulate on
    \code{gradient} (1 or 2 ).
  }
  \item{ \dots }{
    Additional arguments passed to coenoflex.
  }
  \item{ verbose }{
    Show status reports during function call.
  }
}

\details{
  \code{Vegsoup()} is the constructor function to create a prototype
  object of class \code{Vegsoup}. The first four arguments
  (\code{x, y, z, coverscale}) have to be specified. However, if argument
  \code{'x'} inherits from class "SpeciesTaxonomy", argument \code{'z'} can be
  left missing.
  
  The creator function does not allow to specify all slots of an
  \code{Vegsoup*} object, but rather tries to guess some of it's contents.
  For example, if the sites data has variables exactly named longitude and
  latitude those will be used to extract coordinates from. For coordinates
  conforming to \code{proj4string = "+init=epsg:4326"} those have to be
  formatted as decimal degrees! See e.g. \code{\link{char2dms}} to convert
  between formats.

%%\code{Vegsoup} will discard any alphanumeric characters by calling
%%\code{gsub(pattern = "[[:alpha:]]", replacement = "", ...}. Decimals (","
%%and ".") are handled to yield numeric values for columns that can be coerced
%%to \code{as.numeric} (e.\,g. in x$cov if abundance is not ordinal, or for
%%all in y that represent continuous measurement on plot.).

  Function \code{plainsoup} is a utility function that demands a community matrix
  but nothing more. This function is suitable if there is no further
  information on sample plots (sites or environmental data) and the taxonomic concept used.
  In order to build a \code{Vegsoup} object dummy values have to be created.
  Note, those will not be informative! To be exact, the function will create
  a single site variable named \code{"X1"} with all values set to zero.
  \code{colnames(m)} are used as abbreviations (see \code{\link{abbr}}) as well as
  full scientific latin names (see \code{\linkS4class{Taxonomy}}).
  All observations are put on a single layer encoded by argument \code{layer}.
  Because the cover (abundance) scale can not be guessed from argument \code{m}
  a suitable cover scale as object \code{\linkS4class{Coverscale}} needs to be assigned.

  Function \code{instantsoup} is a shorthand when data files are guaranteed to
  conform to some format that can be interpreted by the import functions
  \code{stackSpecies}, respectively \code{stackSites} or are valid to simple
  calls to class creator methods \code{\link{species}}, \code{\link{sites}}
  and \code{\link{taxonomy}} all with signature \code{'character'}. Arguments
  \code{'coverscale'} and \code{'decostand'} can be specified afterwards
  if needed, defaults of class \code{\linkS4class{Vegsoup}} apply otherwise.
  The same is true for argument \code{'proj4string'}. The defaults for
  argument \code{'group'} is to create a dummy integer variable of ones.

  Function \code{coenflex} creates a random community with one or two
  gradients and specified size (number of species and ploits). The \code{coenocline}
  function in package \pkg{coenocliner} offers a complete interface. \code{coenocline}
  objects can be coerced to class \code{Vegsoup} with the \code{as(obj, "Vegsoup")}
  coerce method.
}

\note{
  If you gain errors using \code{instantsoup}, try to create separate objects
  for classes \code{"Species"}, \code{"Sites"} and \code{"Taxonomy"}. You may
  also want to call \code{"SpeciesTaxonomy"} for the first and last objects
  mentioned. Defaults for \code{sep} and \code{dec} passed to \code{read.csv}
  are set according to \code{read.csv2}!
  
  Package coenoflex is currently missing from CRAN.
}

\section{ Warning }{
  All missing values that might result when casting \code{y} will be replaced
  by \code{NA}!

  If sp.points and/or sp.polygons arguments are supplied,
  the objects must have a column named plot.

  When suppling argument \code{proj4string} ensure that
 \code{CRS(proj4string)} works.
}

\value{
An object of class \code{\linkS4class{Vegsoup}}.
}

\author{ Roland Kaiser }

\seealso{
  \code{\link{Vegsoup-class}}
  \code{\link{as.matrix}}
  \code{\link{coverscale}}
  \code{\link{decostand}}
  \code{\link{vegdist}}
}

\examples{
data(barmstein)
x <- barmstein

summary(x)

# plot ordinations of simulated data
x <- coenoflex(100, 50)
plotPCO(x, thresh = 0.2)
plot(metaMDS(as.logical(x)))

# if we omit 'as-logical' and the default 'as.matrix' applies
plot(decorana(x))

# promote to ordinal scale
coverscale(x) <- "ordinal"
table(species(coenoflex())$cov)

# order and coerce to a sparse matrix representation
vegdist(barmstein) <- "bray"
decostand(barmstein) <- "wisconsin"
as(seriation(barmstein, method = "packed"), "sparseMatrix")

# R-mode analysis using mode argument, one of 'Q' or 'R'
as(as.logical(barmstein, mode = "R"), "sparseMatrix")

# simulate 10 plots with 50 species
coenoflex(10, 50)
}

\keyword{ classes }

