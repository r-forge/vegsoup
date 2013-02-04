\name{Sites-class}
\Rdversion{1.1}
\docType{class}
\alias{Sites-class}
\alias{sites,Sites-method}

\title{Class \code{"Sites"}}
\description{
Data representation of sites data in long format.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Sites", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} with three columns named exactly \code{'plot'}, \code{'variable'} and \code{'value'}. }
  }
}
\section{Methods}{
  \describe{
    \item{sites}{\code{signature(obj = "Sites")}: accessor function for slot \code{obj@data} } 
	 }
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{stack.sites}}
\code{\linkS4class{Vegsoup}}
}
\examples{
showClass("Sites")
}
\keyword{classes}
