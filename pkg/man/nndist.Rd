\name{nndist}
\docType{methods}
\alias{nndist}
\alias{nndist,Vegsoup-method}

\title{ Nearest Neighbour Distances }

\description{
  Identifies the nearest neighbour of each plot based on dissimilarity matrix.
}

\usage{
\S4method{nndist}{Vegsoup}(X, ...)
}

\arguments{
  \item{ X }{
    A \code{Vegsoup*} object.
  }
  \item{ \dots }{
    Not used.
  }
}

\details{
Nothing here yet!
}

\value{
  Returns a numeric vector of distances equal to the number of plots. The identified nearest
  neighbouring plot is given as attribute. See \sQuote{Examples}.
}
\author{ Roland Kaiser }
%\references{
%}
\seealso{
\code{\link{outlier}}, \code{\link{as.dist}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")
vegdist(dta) <- "bray"
(res <- nndist(dta))

# access nearest neighbour distances 
attr(res, "neighbour")
}
\keyword{methods}
