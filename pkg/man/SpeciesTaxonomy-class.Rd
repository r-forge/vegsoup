\name{SpeciesTaxonomy-class}
\Rdversion{1.1}
\docType{class}
\alias{SpeciesTaxonomy-class}

\title{Class \code{"SpeciesTaxonomy"}}
\description{
A super class containing classes \code{"Species"} and \code{"Taxonomy"}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("SpeciesTaxonomy", ...)}, where \code{...} expands to slots
  \code{'Species'} and \code{'Taxonomy'}, each of which is a class as
  indicated by the slot name. Note, class names in package \pkg{vegsoup}
  always start with an uppercase letter. When using the \code{new()} method
  to initilaize an object of class \code{"SpeciesTaxonomy"} the corresponding
  function \code{SpeciesTaxonomy} is called internally in order ensure data
  integrity. In this way it might be more transparent for user to call
  the constructor function \code{\link{SpeciesTaxonomy}} directly which will
  also return an object of class \code{"SpeciesTaxonomy"}.
}
\section{Slots}{
  \describe{
    \item{\code{species}:}{Object of class \code{"Species"}
      see \code{\linkS4class{Species}} for details.
    }
    \item{\code{taxonomy}:}{Object of class \code{"Taxonomy"}
      see \code{\linkS4class{Taxonomy}} for details.
    }
  }
}
\section{Methods}{
Unlike classes \code{"Species"} and \code{"Taxonomy"} there exists no initalize method. Function \code{SpeciesTaxonomy} however can be used.
No methods defined with class "SpeciesTaxonomy" in the signature.
}
\author{
Roland Kaiser
}
\seealso{
  \code{\link{SpeciesTaxonomy}},
  \code{\linkS4class{Sites}},
  \code{\linkS4class{Taxonomy}},
  \code{\link{taxval}}
}
\examples{
showClass("SpeciesTaxonomy")

data(testdata)

new("SpeciesTaxonomy", species = species(spc), taxonomy = taxonomy(txa))

#	equivalent and significantly shorter
SpeciesTaxonomy(spc, txa, verbose = TRUE)
}
\keyword{classes}
