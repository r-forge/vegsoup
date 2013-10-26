\name{Species-class}
\Rdversion{1.1}
\docType{class}

\alias{species}
\alias{Species-class}

\alias{[,Species,ANY,ANY,missing-method}

\alias{$,Species-method}

\alias{$<-,Species-method}

\alias{rbind,Species-method}

\alias{species,Species-method}

\alias{species-methods}
\alias{species,character-method}
\alias{species,data.frame-method}
\alias{species,matrix-method}
\alias{species,Species-method}
\alias{species,SpeciesTaxonomy-method}

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
      species observation.
    \item \code{abbr} typically contains unique abbreviations of a scientific
      latin name, however, any character string of potentially unlimited
      length can be used. Syntactic valid names are automatically created
      using \code{\link{make.names}} (e.g. blanks will be dots).
    \item \code{layer} assigns a layer to each observation.
    \item \code{cov} contains an estimate of species cover (abundance or
      performance) coerced to as character, regardless if it is ordinal
      or continuous.
  }

  Besides these mandatory columns and their rigid order it is possible
  for an object of class \code{"Species"} to contain more columns with
  data. Imagine, for example, you have additional information
  (grazed/not grazed) on an observed species in a plot.
}

\section{Slots}{
  \describe{
    \item{ \code{data}: }{
      Object of class \code{"data.frame"} with at least four columns named
      \code{'plot'}, \code{'abbr'}, \code{'layer'} and \code{'cov'}.
    }
  }
}

\section{Methods}{
  \describe{
    \item{ species }{
      \code{signature(obj = "Species")}:
        Accessor method (slot \code{obj@data}).

      \code{signature(obj = "data.frame")}:
        Promote \code{data.frame} to object of class \code{"Species"}.

      \code{signature(obj = "matrix")}:
        Promote \code{matrix} to object of class \code{"Species"}.
        Note, \code{obj} does not need to have \code{dimnames}.

      \code{signature(obj = "character")}:
        Supply a file to be read by \code{read.csv} and promote that object to
        class \code{"Species"}. Remember to pass appropriate arguments to
        \code{\link{read.csv}} that meet the specifications of your file.
    }
    \item{ [ }{
      \code{signature(x = "Species")}:
        Subset parts of an object. Note, indexing that will
        evaluate to an object with less than four columns will fail,
        due to the class inherent data model (see \sQuote{Slots}).
    }
    \item{ $ }{
      \code{signature(x = "Species")}:
        Retreive column by name, or perform replacement.
    }
    \item{ rbind }{
      \code{signature(x = "Species")}:
        \code{rbind} method to combine two or more objects.
    }
  }
}

\section{Details}{
  The initialization step for the class involves some basic checks of data
  integrity. If duplicated species are identified for a specific plot a
  warning is issued and duplicates are dropped automatically. The same applies
  to duplicated species abundances for plots. Finally an ordering of the form
  and \code{order(plot, layer, data)} is performed. None of the first four
  columns can contain \code{NA}, as this will not allow a valid object of the
  class to be built.
}

\section{Note}{
  The design of the class, especially the initialization method, allows data
  that have not been checked for data integrity. Such data might arise when
  field data was entered in spread sheet software. The default behavior is to
  discard all ambiguities in the data (e.g. duplicated observations of species
  in the same plot and the same layer, if there are more than one.).

  A standard procedure to abbreviate scientific names is to use four first
  letters of the generic name and four first letters of the specific epithet
  (see\code{\link{make.cepnames}}). Depending on the number of taxa that need
  to be encoded, another block of four letters might be necessary, in order to
  reference lower taxonomic ranks, or to make non unique abbreviations unique.
}

\author{ Roland Kaiser }

\seealso{
  \code{\linkS4class{SpeciesTaxonomy}}
  and function \code{\link{SpeciesTaxonomy}},
  \code{\linkS4class{Taxonomy}},
  \code{\linkS4class{Sites}},
  \code{\linkS4class{Vegsoup}},
  \code{\link{stack.species}}
}

\examples{
require(vegsoup)
showClass("Species")

data(testdata)

new("Species", data = spc)

class(spc)
species(spc)
obj <- species(as.matrix(spc))

# get slot data from object
class(species(obj))

# combine objects
data(bigtestdata)

obj2 <- species(spc.big)

obj <- rbind(obj, obj2)

}
\keyword{ classes }
