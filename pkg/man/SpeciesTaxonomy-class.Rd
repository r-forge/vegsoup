\name{SpeciesTaxonomy-class}
\Rdversion{1.1}
\docType{class}

\alias{SpeciesTaxonomy-class}
\alias{species,SpeciesTaxonomy-method}
\alias{species<-,SpeciesTaxonomy,data.frame-method}
\alias{species<-,SpeciesTaxonomy,Species-method}
\alias{taxonomy,SpeciesTaxonomy-method}
\alias{taxonomy<-,SpeciesTaxonomy,data.frame-method}
\alias{taxonomy<-,SpeciesTaxonomy,Taxonomy-method}
\alias{rbind,SpeciesTaxonomy-method}

\title{ Class \code{"SpeciesTaxonomy"} }

\description{
  A super class containing classes \code{"Species"} and \code{"Taxonomy"}.
}

\usage{
\S4method{species<-}{SpeciesTaxonomy,Species}(x) <- value
\S4method{species<-}{SpeciesTaxonomy,data.frame}(x) <- value
\S4method{taxonomy<-}{SpeciesTaxonomy,Taxonomy}(x) <- value
\S4method{taxonomy<-}{SpeciesTaxonomy,data.frame}(x) <- value
}

\arguments{
\item{ x }{
\code{'Vegsoup'} object.
}
\item{value}{
class. Either \code{'Species'}, \code{'Taxonomy'} or \code{'data.frame'}.
}
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("SpeciesTaxonomy", ...)}, where \code{...} expands to slots
  \code{'species'} and \code{'taxonomy'}, each of which is a class as
  indicated by the slot name. Note, class names in package \pkg{vegsoup}
  always start with an uppercase letter. When using the \code{new()} method
  to initialize an object of class \code{"SpeciesTaxonomy"} the corresponding
  function \code{SpeciesTaxonomy} is called internally in order to ensure data
  integrity. In this way it might be more transparent for users to call
  the constructor function \code{\link{SpeciesTaxonomy}} directly, which will
  return an object of class \code{"SpeciesTaxonomy"} aswell.
}

\section{Slots}{
  \describe{
    \item{\code{species}:}{Object of class \code{"Species"},
      see \code{\linkS4class{Species}} for details.
    }
    \item{\code{taxonomy}:}{Object of class \code{"Taxonomy"},
      see \code{\linkS4class{Taxonomy}} for details.
    }
  }
}

\section{Methods}{
  Unlike classes \code{"Species"} and \code{"Taxonomy"} there exists no
  custom initialize method. Therefore, function \code{SpeciesTaxonomy} should
  be used in cases where the data integrity is unsure.

  Subsetting methods are provided for classes \code{\linkS4class{Species}},
  and \code{\linkS4class{Taxonomy}}, but not for this class. It is possible,
  however, to replace one of the slots by the respective replacement method,
  which will take care about subsetting of the remaining slot
  (see \sQuote{Methods for "species<-"} and
  \sQuote{Methods for "taxonomy<-"}).

  \describe{
    \item{ species }{
      \code{signature(obj = "SpeciesTaxonomy")}:
        Accessor method for slot \code{species}
        returning a \code{data.frame}
    }
    \item{ taxonomy }{
      \code{signature(obj = "SpeciesTaxonomy")}:
        Accessor method for slot \code{taxonomy}
        returning a \code{data.frame}.
    }
    \item{ rbind }{
      \code{signature(x = "SpeciesTaxonomy")}:
      \code{rbind} method to combine two or more objects.
    }
  }
}

\section{Methods for "species<-"}{
  \describe{
    \item{ obj = "SpeciesTaxonomy", value = "Taxonomy" }{
      subset slot \code{species} as to contain anly the taxa given by an
      object of class \code{"Taxonomy"}, which by itself will replace
      slot \code{taxonomy}.
    }
    \item{ obj = "SpeciesTaxonomy", value = "data.frame" }{
      subset slot \code{species} as above, but by an object of class
      \code{"data.frame"}. The latter is coerced to class \code{"Taxonomy"}.
    }
  }
}

\section{Methods for "taxonomy<-"}{
  \describe{
    \item{ obj = "SpeciesTaxonomy", value = "Species" }{
      subset slot \code{taxonomy} as to contain anly the taxa given by an
      object of class \code{"Species"}, which by itself will replace
      slot \code{species}.
    }
    \item{ obj = "SpeciesTaxonomy", value = "data.frame" }{
      subset slot \code{taxonomy} as above, but by an object of class
      \code{"data.frame"}. The latter is coerced to class \code{"Species"}.
    }
  }
}
\author{ Roland Kaiser }
\seealso{
  \code{\link{SpeciesTaxonomy}},
  \code{\linkS4class{Species}},
  \code{\linkS4class{Sites}},
  \code{\linkS4class{Taxonomy}},
  \code{\link[vegdata:taxval]{taxval}} inpackage \pkg{vegdata}.
}
\examples{
require(vegsoup)
showClass("SpeciesTaxonomy")

data(testdata)

new("SpeciesTaxonomy", species = species(spc), taxonomy = taxonomy(txa))

# equivalent and significantly shorter
obj <- SpeciesTaxonomy(spc, txa, verbose = TRUE)

# modify and subset an object by replacing slots
obj1 <- obj2 <- obj3 <- obj

# the first 10 species
taxonomy(obj1) <- taxonomy(obj1)[1:10, ]
obj1
# specific genus
taxonomy(obj2) <- taxonomy(obj2)[grep("Carex", taxonomy(obj)$taxon), ]
obj2
# first three
species(obj3) <- species(obj3)[c(1:10, 15:20), ]
obj3

# combine objects
data(bigtestdata)

obj1 <- SpeciesTaxonomy(spc, txa)
obj2 <- SpeciesTaxonomy(spc.big, txa.big)

obj <- rbind(obj1, obj2)

# assign object of class SpeciesTaxonomy to an existing Vegsoup* object
dta <- Vegsoup(obj, sts, coverscale = "braun.blanquet")
#  not implemented yet!
## Species(dta) <- obj3

}
\keyword{ classes }
