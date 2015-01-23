\encoding{UTF-8}

\docType{class}

\name{SpeciesTaxonomy-class}

\alias{SpeciesTaxonomy-class}

\alias{species,SpeciesTaxonomy-method}

\alias{species<-,SpeciesTaxonomy,Species-method}
\alias{species<-,SpeciesTaxonomy,data.frame-method}

\alias{taxonomy,SpeciesTaxonomy-method}

\alias{taxonomy<-}
\alias{taxonomy<-,SpeciesTaxonomy,Species-method}
\alias{taxonomy<-,SpeciesTaxonomy,Taxonomy-method}
\alias{taxonomy<-,SpeciesTaxonomy,data.frame-method}

\alias{[,SpeciesTaxonomy,ANY,ANY,missing-method}

\alias{$,SpeciesTaxonomy-method}

\alias{bind,SpeciesTaxonomy-method}

\title{ SpeciesTaxonomy Class }

\description{
  A super class containing classes \code{"Species"} and \code{"Taxonomy"}. The class
  guarantees that both objects match by their primary key (\code{'abbr'}).
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
  return an object of class \code{"SpeciesTaxonomy"} as well.
}

\section{Slots}{
  \describe{
    \item{\code{species}:}{Object of class \code{"Species"},
      see class \code{\linkS4class{Species}} for details.
    }
    \item{\code{taxonomy}:}{Object of class \code{"Taxonomy"},
      see class \code{\linkS4class{Taxonomy}} for details.
    }
  }
}

\section{ Methods }{
  Unlike classes \code{"Species"} and \code{"Taxonomy"} there exists no
  custom initialize method. Therefore, function \code{SpeciesTaxonomy} should
  be used in cases where the data integrity is unsure.

  Sub-setting methods are provided for classes \code{\linkS4class{Species}},
  and \code{\linkS4class{Taxonomy}}, but not for this class. It is possible,
  however, to replace one of the slots by the respective replacement method,
  which will take care about sub-setting of the remaining slot
  (see \sQuote{Methods for "species<-"} and \sQuote{Methods for "taxonomy<-"}).

  \describe{
 
    \item{ species }{
      \code{ signature(obj = "SpeciesTaxonomy") }:
        Accessor method for slot \code{species}
        returning a \code{data.frame}
    }

    \item{ taxonomy }{
      \code{ signature(obj = "SpeciesTaxonomy") }:
        Accessor method for slot \code{taxonomy}
        returning a \code{data.frame}.
    }

    \item{ bind }{
      \code{ signature(x = "SpeciesTaxonomy") }:
      \code{ bind } method to combine two or more objects.
    }

    \item{ $ }{
      \code{signature(x = "SpeciesTaxonomy")}:
        retrieve column name of slot species (see \code{\linkS4class{Species}}).
    }	

    \item{ [ }{
      \code{signature(x = "SpeciesTaxonomy")}:
        subset parts of an object (see \code{\linkS4class{Species}}).
    }
	
    \item{ signature(obj = "SpeciesTaxonomy", value = "Taxonomy") }{
      subset slot \code{species} as to contain only the taxa given by an
      object of class \code{"Taxonomy"}, which by itself will replace
      slot \code{taxonomy}.
    }

    \item{ signature(obj = "SpeciesTaxonomy", value = "data.frame") }{
      subset slot \code{species} as above, but by an object of class
      \code{"data.frame"}. The latter is coerced to class \code{"Taxonomy"}.
    }
 
    \item{ signature(obj = "SpeciesTaxonomy", value = "Species") }{
      subset slot \code{taxonomy} as to contain only the taxa given by an
      object of class \code{"Species"}, which by itself will replace
      slot \code{species}.
    }
 
    \item{ signature(obj = "SpeciesTaxonomy", value = "Taxonomy") }{
      experimental!
    }

    \item{ signature(obj = "SpeciesTaxonomy", value = "data.frame") }{
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
data(barmstein)
x <- barmstein

# extract from object for demonstration
s <- species(species(x))    # returns a data.frame
t <- taxonomy(taxonomy(x))  # returns a data.frame

# using the new() inititialization method
st0 <- new("SpeciesTaxonomy")
slotNames(st0)              # see slots names
st0@species <- species(s)   # species method for class data.frame, returns a 'Species' object
st0@taxonomy <- taxonomy(t) # taxonomy method for class data.frame, returns a 'Taxonomy' object
st0

# supply slot in call to new()
new("SpeciesTaxonomy", species = species(s), taxonomy = taxonomy(t))

# equivalent. preferred and significantly shorter and more transparent interface
st2 <- SpeciesTaxonomy(s, t, verbose = TRUE)
identical(st0, st2)

# modify and subset an object by replacing slots
# in this way we can take subset of species
obj <- st2
obj1 <- obj2 <- obj3 <- obj

# the first 10 species
( taxonomy(obj1) <- taxonomy(obj1)[1:10, ] )

# specific genus
( taxonomy(obj2) <- taxonomy(obj2)[grep("Carex", taxonomy(obj)$taxon), ] )

# get a subset of species, e.g. those occuring in the shrub layer 'sl'
i <- species(obj3)$layer == "sl"
( species(obj3) <- species(obj3)[i, ] )


# combine objects
obj1 <- SpeciesTaxonomy(s, t)

data(windsfeld)
x <- windsfeld
s2 <- species(x)  # returns a class 'Species'
t2 <- taxonomy(x) # returns a class 'Taxonomy'
obj2 <- SpeciesTaxonomy(s2, t2)

( obj <- bind(obj1, obj2) )

# assign object of class SpeciesTaxonomy to an existing Vegsoup* object
\dontrun{
# not implemented yet!
# Species(x) <- obj3
}
}
\keyword{ classes }

