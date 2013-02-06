\name{Species-class}
\Rdversion{1.1}
\docType{class}
\alias{Species-class}
\alias{species}
\alias{species,Species-method}
\alias{species,data.frame-method}
\alias{species,matrix-method}

\title{Class \code{"Species"}}

\description{
  Data representation of species data in \emph{long format}, where each
  observation of a species in a particular plot is represented by one row
  spanning at least four columns.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Species", ...)},
  where \code{...} expands to slot \code{'data'}. Using the \code{species}
  method with appropriate objects is also fast and convenient
  (see \sQuote{Methods}).

  The \code{data} slot holds a \code{"data.frame"} with four columns, all of
  type \code{character} and with names \code{plot}, \code{abbr}, \code{layer}
  and \code{cov}. The meaning of these columns is as detailed below:

  \itemize{
    \item \code{plot} contains the plot identifier associated with each
      species observation. \item \code{abbr} typically contains unique
      abbreviations of a scientific latin name, however, any character
      string of potentially unlimited length can be used. In more detail,
      a standard procedure to abbreviate scientific names is to use
      four first letters of the generic name and four first letters
      of the specific epithet (see\code{\link{make.cepnames}}).
      Depending on the number of taxa that need to be encoded, another block
      of four letters might be necessary, in order to reference lower
      taxonomic ranks, or to make non unique abbreviations unique.
    \item \code{layer} assigns a layer to each observation.
    \item \code{cov} contains an estimate of species cover (abundance or
      performance) coerced to as character, regardless if it is ordinal
      or continuous.
  }
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}
      with at least four columns named
      \code{'plot'}, \code{'abbr'}, \code{'layer'} and \code{'cov'}.
    }
  }
}
\section{Methods}{
  \describe{
    \item{ species }{
      \code{signature(obj = "Species")}:
      Accessor function (slot \code{obj@data}).

      \code{signature(obj = "SpeciesTaxonomy")}:
      Accessor function for objects of class \code{SpeciesTaxonomy}.

      \code{signature(obj = "data.frame")}:
      Method to create object from class \code{data.frame}.

      \code{signature(obj = "matrix")}:
      Method to create object from class \code{matrix}.
      Note, \code{obj} does not need to have \code{dimnames}.
    }
  }
}
\section{Details}{
  The initialization step for the class involves some basic checks of data
  integrity. If duplicated species are identified for a specific plot a
  warning is issued and duplicates are dropped automatically. The same applies
  to duplicated species abundances for plots. Finally an ordering of the form
  and \code{order(plot, layer, data)} is performed.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{stack.species}},
\code{\linkS4class{SpeciesTaxonomy}}
and function \code{\link{SpeciesTaxonomy}},
\code{\linkS4class{Taxonomy}},
\code{\linkS4class{Sites}},
\code{\linkS4class{Vegsoup}}
}
\examples{
showClass("Species")

data(testdata)

new("Species", data = spc)

class(spc)
species(spc)
obj <- species(as.matrix(spc))

# get slot data from object
class(species(obj))
}
\keyword{classes}
