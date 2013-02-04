\name{Species-class}
\Rdversion{1.1}
\docType{class}
\alias{Species-class}
\alias{species,Species-method}

\title{Class \code{"Species"}}
\description{
Data representation of species data in long format.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Species", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} with at least four columns named exactly \code{'plot'}, \code{'abbr'}, \code{'layer'} and \code{'cov'}. }
  }
}
\section{Methods}{
  \describe{
    \item{species}{\code{signature(obj = "Species")}: accessor function for slot \code{obj@data} }
	 }
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{stack.species}}
\code{\linkS4class{Vegsoup}}
}
\examples{
showClass("Species")
}
\keyword{classes}
