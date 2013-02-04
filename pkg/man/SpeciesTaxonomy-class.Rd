\name{SpeciesTaxonomy-class}
\Rdversion{1.1}
\docType{class}
\alias{SpeciesTaxonomy-class}

\title{Class \code{"SpeciesTaxonomy"}}
\description{
A super class containing class \code{"Species"} and \code{"Taxonomy"}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpeciesTaxonomy", ...)}.
The preferred way is to use the constructor function \code{\link{SpeciesTaxonomy}}.
}
\section{Slots}{
  \describe{
    \item{\code{species}:}{Object of class \code{"Species"} see \code{\linkS4class{Species}} for details.}
    \item{\code{taxonomy}:}{Object of class \code{"Taxonomy"} see \code{\linkS4class{Taxonomy}} for details.}
  }
}
\section{Methods}{
No methods defined with class "SpeciesTaxonomy" in the signature.
Objects can be created by calls to the function \code{\link{SpeciesTaxonomy}}
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{SpeciesTaxonomy}}
}
\examples{
showClass("SpeciesTaxonomy")
}
\keyword{classes}
