\encoding{UTF-8}

\name{abundant}
\docType{methods}

\alias{abundant}
\alias{abundant,Vegsoup-method}

\title{
Return Abundant Species According to a Threshold Value.
}
\description{
Return species meeting the abundance threshold given by argument 
\code{treshold} applied to \code{constancy(obj)} along with median abundance 
(see \code{\link{quantile}}).
}
\usage{
\S4method{abundant}{Vegsoup}(obj, threshold = 75, layers = FALSE, ...)
}
\arguments{
  \item{ obj }{
    An object inheriting from class \code{VegsoupPartition}. 
  }
  \item{ threshold }{
    numeric. Threshold value.
  }
  \item{ layers }{
    logical. If \code{FALSE} do not take layers into account.
  }
  \item{ \dots }{
    currently not used.
    %additional arguments passed to \code{\link{as.dist}}. 
  }
}

%\details{
%}

\value{
A \code{data.frame}.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{constancy}}, \code{\link{decode}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

abundant(x) # with default values
abundant(x, treshold = 50) # treshold set to 50% constancy
abundant(x, treshold = 75, layers = TRUE) # include layers

x <- VegsoupPartition(barmstein, k = 2)
abundant(x) # threshold applied over all partitions
abundant(partition(x, 1)) # for a selected partition
abundant(partition(x, 2), layers = TRUE) # for a selected partition with layers
}

\keyword{ methods }

