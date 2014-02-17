\encoding{UTF-8}
\name{Species-class}
\Rdversion{1.1}
\docType{class}

\alias{Species-class}

\alias{[,Species,ANY,ANY,missing-method}

\alias{species}

\alias{$,Species-method}

\alias{$<-,Species-method}

\alias{rbind,Species-method}

\alias{species,Species-method}

\alias{species,character-method}
\alias{species,data.frame-method}
\alias{species,matrix-method}
\alias{species,Species-method}
\alias{species,VegsoupVerbatim-method}

\title{ Species Class}

\description{
  Data representation of species data in \emph{long format}, where each
  observation of a species in a particular plot is represented by one row
  spanning at least four columns.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Species", ...)},
  where \code{...} expands to slot \code{'data'}. Using the \code{species}
  method with appropriate objects is also convenient (see \sQuote{Methods}).
  The \code{\link{stackSpecies}} function is useful when the data is in \emph{wide}
  format (plot by species matrix).

}

\section{Slots}{
  \describe{
  \item{ \code{data}: }{
    Object of class \code{"data.frame"} with at least four columns named
    \code{'plot'}, \code{'abbr'}, \code{'layer'} and \code{'cov'}, all of
    type \code{character}. The meaning of these columns is as detailed below:

    \itemize{
    \item \code{plot} contains the plot identifier associated with each
      species observation, any numeric values are coerced to characters.
    \item \code{abbr} typically contains unique abbreviations of a scientific
      latin name, although, any character string might be used (the full species name for example).
      It's handy however, to keep \code{'abbr'} short and intuitive. In any case, syntactic valid
      names are automatically created using \code{\link{make.names}} (e.g. blanks will be dots).
    \item \code{layer} assigns a stratum/layer to each observation, even if they are all the same
      (e.g. the data includes only a single herb layer)
    \item \code{cov} contains an estimate of species abundance or performance (e.g. plant cover)
      coerced to as character, regardless if it is ordinal (Braun-Blanquet scale),
      continuous (frequencies or counts) or simple presence.
    
    Besides these mandatory columns and their rigid names and order, it is possible
    for an object of class \code{"Species"} to contain more columns with
    data. Imagine, for example, additional useful information for an observed species
    in a particular plot (plant size, flowering/not flowering, etc.) is available.
   }
  }
  }
}

\section{Methods}{
  \describe{
    \item{ species }{
      There exists \code{species} methods for a number of R base classes:

      \code{signature(obj = "data.frame")}:
        Promote \code{data.frame} to object of class \code{"Species"}.

      \code{signature(obj = "matrix")}:
        Promote \code{matrix} to object of class \code{"Species"}.
        Note, \code{obj} does not need to have \code{dimnames}, but order matters.

      \code{signature(obj = "character")}:
        Supply a file to be read by \code{read.csv} and promote that object to
        class \code{"Species"}. Remember to pass appropriate arguments to
        \code{\link{read.csv}} that meet the specifications of your file (e.g. argument \code{sep}).

      The following classes defined in the vegsoup package have a \code{species} method:

      \code{signature(obj = "Species")}:
        Accessor method (slot \code{obj@data}).

      \code{signature(obj = "VergsoupVerbatim")}:
        Supply an \code{"VegsoupVerbatim"} and promote that object to
        class \code{"Species"}.
    }
    \item{ [ }{
      \code{signature(x = "Species")}:
        Subset parts of an object. Note, indexing that will
        evaluate to an object with less than four columns will fail,
        due to the class inherent data model (see \sQuote{Slots}).
    }
    \item{ $ }{
      \code{signature(x = "Species")}:
        Retrieve column by name (see \sQuote{Slots}), or perform replacement.
    }
    \item{ rbind }{
      \code{signature(x = "Species")}:
        \code{rbind} method to combine two or more objects. See \code{\link{verbatim}}
        for an example.
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

  The design of the the class initialization method allows for data
  that have not been checked for data stringency. Such data might arise when
  field data was digitized without the help of a database system, for example, by
  using a spread sheet software. The default behavior is to  discard all ambiguities
  in the data (e.g. duplicated observations of species in the same plot
  and/or in the same stratum).

  A standard procedure to abbreviate scientific names is to use four first
  letters of the generic name and four first letters of the specific epithet
  (see \code{\link{make.cepnames}}). Depending on the number of taxa that need
  to be encoded, another block of four letters might be necessary, in order to
  reference lower taxonomic ranks, or to make non unique abbreviations unique.
}

\author{ Roland Kaiser }

\seealso{
  class \code{\linkS4class{SpeciesTaxonomy}}
  and function \code{\link{SpeciesTaxonomy}},
  \code{\linkS4class{Taxonomy}},
  \code{\linkS4class{Sites}},
  \code{\linkS4class{Vegsoup}},
  \code{\link{stackSpecies}}
}

\examples{
require(vegsoup)

showClass("Species")

data(barmstein)

# retrieve data.frame from an Vegsoup object
spc <- Species(barmstein)

class(spc)

# the data slot
class(species(spc))

# matrix method
s1 <- species(as.matrix(spc))

# get slot data from object, a simple data.frame
class(species(s1))

# combine two objects
data(windsfeld)
s2 <- species(Species(windsfeld))
( s <- rbind(s1, s2) )
}
\keyword{ classes }
