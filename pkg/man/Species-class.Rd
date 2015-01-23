\encoding{UTF-8}

\name{Species-class}
\Rdversion{1.1}
\docType{class}

\alias{Species-class}

\alias{species}

\alias{nrow,Species-method}
\alias{dimnames,Species-method}

\alias{[,Species,ANY,ANY,missing-method}

\alias{$,Species-method}

\alias{$<-,Species-method}

\alias{bind,Species-method}

\alias{species,Species-method}

\alias{species,character-method}
\alias{species,data.frame-method}
\alias{species,matrix-method}
\alias{species,Species-method}
\alias{species,VegsoupVerbatim-method}
\alias{bind,Species-method}

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
    \item \code{"plot"} contains the plot identifier associated with each
      species observation, any numeric values are coerced to characters.
    \item \code{"abbr"} typically contains unique abbreviations of a scientific
      latin name, although, any character string might be used (the full species name for example).
      It's handy however, to keep \code{abbr} short and intuitive. In any case, syntactic valid
      names are automatically created using \code{\link{make.names}} (e.g. blanks will be dots).
    \item \code{"layer"} assigns a stratum/layer to each observation, even if they are all the same
      (e.g. the data includes only a single herb layer)
    \item \code{"cov"} contains an estimate of species abundance or performance (e.g. plant cover)
      coerced to as character, regardless if it is ordinal (Braun-Blanquet scale),
      continuous (frequencies or counts) or simple presence.
   }
   
   The above outlined data columns must be available in order to create a \code{"Species"} object
   and the species method below relies they are present, named or in the order above.
   
   Besides these mandatory columns and their rigid names and order, it is possible
   for an object of class \code{"Species"} to contain more columns with
   data. Imagine, for example, additional useful information for an observed species
   in a particular plot (plant size, flowering/not flowering, etc.) is available.

  }
  }
}

\section{Methods}{
  \describe{
    \item{ species }{
      There exists \code{species} methods for a number of R base classes:

      \code{signature(obj = "data.frame")}:
        promote \code{data.frame} to object of class \code{"Species"}.

      \code{signature(obj = "matrix")}:
        promote \code{matrix} to object of class \code{"Species"}.
        Note, \code{obj} does not need to have \code{dimnames}, but order matters.

      \code{signature(obj = "character")}:
        supply a file to be read by \code{read.csv} and promote that object to
        class \code{"Species"}. Remember to pass appropriate arguments to
        \code{\link{read.csv}} that meet the specifications of your file (e.g. argument \code{sep}).

      The following classes defined in the vegsoup package have a \code{species} method:

      \code{signature(obj = "Species")}:
        accessor function (\emph{slot "data"}).
		Returns class \code{data.frame}.

      \code{signature(obj = "SpeciesTaxonomy")}:
        accessor function (\emph{slot "taxonomy"}).
		Returns class \code{Species}.
		
      \code{signature(obj = "Vegsoup")}:
        accessor method for \emph{slot "Species"}, equals \code{species(obj)} and returns
        an object of class \code{"Species"}. Use \code{species(species(obj))} to get the
        \emph{slot data}. \code{species(obj)@data} should be avoided.

      \code{signature(obj = "VergsoupVerbatim")}:
        promote \code{"VegsoupVerbatim"} object to class \code{"Species"}.

    }

    \item{ species<- }{
      \code{signature(obj = "Vegsoup", value = "Species")}:
        assign (possibly subset) with object of class \code{"Species"}. 
    }

    \item{ nrow }{
      \code{signature(x = "Species")}:
        the number of rows. Each observation of a species in plot (stratum-replicate)
        constitutes a row.
    }

    \item{ dimnames }{
      \code{signature(x = "Species")}:
        non unique dimnames. The dimnames that are constructed by pasting plot and layer collapsed
        with \code{@}. Not to be called by the user.
    }

    \item{ [ }{
      \code{signature(x = "Species")}:
        subset parts of an object. Note, indexing that will
        evaluate to an object with less than four columns will fail,
        due to the class inherent data model (see \sQuote{Slots}).
    }

    \item{ $ }{
      \code{signature(x = "Species")}:
        retrieve column by name (see \sQuote{Slots}), or perform replacement.
    }

    \item{ bind }{
      \code{signature(x = "Species")}:
        \code{bind} method to combine two or more objects. See \code{\link{verbatim}}
        for an example.
    }

  }
}

\section{Details}{
  The initialization step for the class involves some basic checks of data
  integrity. If duplicated species are identified for a specific plot a
  warning is issued and duplicates are dropped automatically. The same applies
  to duplicated species abundances for plots. Finally an ordering of the form
  \code{order(plot, layer, data)} is performed during initialization. None of
  the first four columns can contain \code{NA}, as this will not allow a valid
  object of the class to be built.

  The design of the the class initialization method allows for data
  that have not been checked for data stringency. Such data might arise when
  field data was digitized without the help of a database system, for example, by
  using spread sheet software. The default behavior is to  discard all ambiguities
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

# for demonstration we first retrieve the slot "Species" of an existing Vegsoup object
spc <- species(barmstein)
class(spc)

# the data slot of a Species object can be accessed by
spc <- species(spc)
class(spc)

# this can also be written as
spc <- species(species(barmstein))

# matrix method to promote to class "Species"
s1 <- species(as.matrix(spc))
class(s1)

# combine two objects
data(windsfeld)
s2 <- species(windsfeld)
( s <- bind(s1, s2) )
}
\keyword{ classes }

