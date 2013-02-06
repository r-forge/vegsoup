\name{Taxonomy-class}
\Rdversion{1.1}
\docType{class}
\alias{Taxonomy-class}
\alias{[,Taxonomy-method}
\alias{taxonomy,Taxonomy-method}
\alias{taxonomy,data.frame-method}
\alias{taxonomy,matrix-method}


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
    }
    \item{ [ }{
      \code{signature(x = "Taxonomy")}:
        Subset parts of an object. Note, indexing that will
        evaluate to an object with less than two columns will fail,
        due to the class inherent data model (see \sQuote{Slots}).
    }
  }
}
\author{
Roland Kaiser
}
\seealso{
\code{\linkS4class{SpeciesTaxonomy}}
and function \code{\link{SpeciesTaxonomy}},
\code{\linkS4class{Species}},
\code{\linkS4class{Sites}}
\code{\link{tax}}
}
\examples{
showClass("Taxonomy")

data(testdata)

new("Taxonomy", data = txa)

class(txa)
taxonomy(txa)
obj <- taxonomy(as.matrix(txa))

#	get slot data from object
class(taxonomy(txa))
}
\keyword{classes}
