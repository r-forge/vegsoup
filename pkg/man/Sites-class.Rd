\name{Sites-class}
\Rdversion{1.1}
\docType{class}
\alias{Sites-class}
\alias{sites,Sites-method}

\title{Class \code{"Sites"}}

\description{
  Data representation of sites data in \emph{long format} where
  each observation on a particular plot (environmental variables,
  stand paarameters) is represented by one row.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Sites", ...)},
  where \code{...} expands to slot \code{'data'}. Using the
  \code{sites} method with appropriate objects is also fast and convenient
  (see \sQuote{Methods}).

  The \code{data} slot holds a \code{"data.frame"} with three columns,
  all of type \code{character} and with names \code{plot}, \code{variable}
  and \code{value}. The meaning of these columns is as detailed below:

  \itemize{
    \item \code{plot} contains the plot identifier associated with each
      observation.
    \item \code{variable} contains a name for the measured variable.
    \item \code{value} holds the value of a particular variable.
}

}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"}
      with three columns named
      \code{'plot'}, \code{'variable'} and \code{'value'}. }
  }
}
\section{Methods}{
  \describe{
    \item{ sites }{
      \code{signature(obj = "Sites")}:
      Accessor function (slot \code{obj@data}).

      \code{signature(obj = "data.frame")}:
      Method to create object from class \code{data.frame}.

      \code{signature(obj = "matrix")}:
      Method to create object from class \code{matrix}.
      Note, \code{obj} does not need to have \code{dimnames}.
    }
  }
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{stack.sites}}
\code{\linkS4class{Species}}
\code{\linkS4class{Vegsoup}}
}
\examples{
showClass("Sites")

showClass("Species")

data(testdata)

new("Sites", data = sts)

class(sts)
sites(sts)
obj <- sites(as.matrix(sts))

# get slot data from object
class(sites(sts))
}
\keyword{classes}
