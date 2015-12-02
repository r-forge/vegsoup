\encoding{UTF-8}

\name{layer}
\docType{methods}

\alias{layer}
\alias{layer,Vegsoup-method}

\alias{layernumber}
\alias{layernumber,Vegsoup-method}

\title{ Return Stratum/Layer }

\description{
Simply return the layer for each observation in \emph{slot species}.
}

\usage{
\S4method{layer}{Vegsoup}(obj, ...)
\S4method{layernumber}{Vegsoup}(obj, ...)
}

\arguments{
  \item{ obj }{
	\code{Vegsoup*} object
  }
  \item{ \dots }{
	passed from to methods.
  }
}

\details{
  Method \code{layer} simply returns the layer for each observation, equivalent
  to \code{species(obj)$layer}. \code{layernumber} returns layer assignments as
  an ordered integer.
}

\value{
  \code{layer} and \code{layernumber} return a vector equal in length to
  \code{nrow(species(obj))}.
}

\author{ Roland Kaiser }

\note{
  The function will be renamed to avoid typos with method \code{layers} or
  even dropped -- it is not very useful and simple minded?
}

\seealso{
\code{\link{layers}}, \code{\linkS4class{Vegsoup}}
}

\examples{
data(barmstein)
x <- barmstein

# check the number of occurences per layer
table(layer(x))

# alternative
table(species(x)$layer)

# layer as integer
layernumber(x) 
}
\keyword{ methods }

