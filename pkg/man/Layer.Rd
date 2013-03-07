\name{Layer}
\alias{Layer}

\title{ Return Layer for Each Species Observation of Vegsoup* objects }
\description{
Simply return the layer for each observation.
}
\usage{
Layer(obj, ...)
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object
  }
  \item{ \dots }{
    passed from to methods.
  }
}
\details{
  Simply returns the layer for each observation, equivalent to
  \code{Species(obj)\$layer}.
}
\value{
  \code{Layer} returns a character vector equal in length to
  \code{nrow(Species(obj)}}
\author{
Roland Kaiser %(\email{kardinal.eros@gmail.com}
}
\note{
  The function will be renamed to avoid typos with method \code{Layers} or
  even dropped -- it is not very useful and simple minded?.
}
\seealso{
  \code{\link{Layers}},
  \code{\linkS4class{Vegsoup}}
}
\examples{
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# check the number of occurences per layer
table(Layer(dta))

# alternative
table(Species(dta)$layer)
}
\keyword{ methods }
