\name{Taxonomy-class}
\Rdversion{1.1}
\docType{class}
\alias{taxonomy}

\alias{Taxonomy-class}

\alias{[,Taxonomy,ANY,ANY,missing-method}

\alias{$,Taxonomy-method}

\alias{rbind,Taxonomy-method}

\alias{Taxonomy<-}
\alias{taxonomy<-,SpeciesTaxonomy,Taxonomy-method}

\alias{Taxonomy}

\alias{taxonomy-methods}
\alias{taxonomy,character-method}
\alias{taxonomy,data.frame-method}
\alias{taxonomy,matrix-method}
\alias{taxonomy,SpeciesTaxonomy-method}
\alias{taxonomy,Taxonomy-method}

\title{Class \code{"Taxonomy"}}
\description{
Data representation of taxonomic reference list.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Taxonomy", ...)},
  where \code{...} expands to slot \code{'data'}. Using the \code{taxonomy}
  method with appropriate objects is also fast and convenient
  (see \sQuote{Methods}).

  \itemize{
    \item \code{abbr} contains unique abbreviations of a scientific latin name
      that translate those of class \code{\linkS4class{Species}}.
    \item \code{taxon} the full scientific latin name.
  }
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} with at least
      two columns named \code{'abbr'} and \code{'taxon'}. }
  }
}
\section{Methods}{
  \describe{
    \item{ taxonomy }{
      \code{signature(obj = "Taxonomy")}:
        Accessor function (slot \code{obj@data}).
      
      \code{signature(obj = "SpeciesTaxonomy")}:
        Accessor function for objects of class \code{SpeciesTaxonomy}.
      
      \code{signature(obj = "data.frame")}:
        Promote \code{data.frame} to object of class \code{"Taxomomy"}.

      \code{signature(obj = "matrix")}:
        Promote \code{matrix} to object of class \code{"Taxomomy"}.
        Note, \code{obj} does not need to have \code{dimnames}.
      \code{signature(obj = "character")}:
        Supply a file to be read by \code{read.csv} and promote that object to
        class \code{"Species"}. Remember to pass appropriate arguments to
        \code{\link{read.csv}} that meet the specifications of your file.
    }
    \item{ [ }{
      \code{signature(x = "Taxonomy")}:
        Subset parts of an object. Note, indexing that will
        evaluate to an object with less than two columns will fail,
        due to the class inherent data model (see \sQuote{Slots}).
    }
    \item{ rbind }{
      \code{signature(x = "Taxonomy")}:
        \code{rbind} method to combine two or more objects. If there are any
        duplicates in the \code{'abbr'} key column only one instance is kept,
        the first one!.
    }
  }
}
\author{ Roland Kaiser }
\seealso{
  \code{\linkS4class{SpeciesTaxonomy}}
  and function \code{\link{SpeciesTaxonomy}},
  \code{\linkS4class{Species}},
  \code{\linkS4class{Sites}}
  \code{\link[vegdata:tax]{tax}} in package \pkg{vegdata}
}
\examples{
require(vegsoup)
showClass("Taxonomy")

data(testdata)

new("Taxonomy", data = txa)

class(txa)
taxonomy(txa)
obj <- taxonomy(as.matrix(txa))

#	get slot data from object
class(taxonomy(txa))

# combine objects
data(bigtestdata)

obj2 <- taxonomy(txa.big)

rbind(obj, obj2)
}
\keyword{ classes }
