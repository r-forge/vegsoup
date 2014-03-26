\encoding{UTF-8}

\name{VegsoupPartitionFidelity-class}
\Rdversion{1.1}
\docType{class}

\alias{VegsoupPartitionFidelity-class}

\title{ VegsoupPartitionFidelity Class }

\description{
A class extending calss \code{"VegsoupPartition"}.
}

\section{ Objects from the Class }{
Objects can be by calls to the fucntion \code{Fidelity}.

}

\section{Slots}{
  Class slots extending \code{'VegsoupPartition'}
  \describe{
    \item{\code{stat}:}{
      Object of class \code{"matrix"} containing the fidelity values.
     }
    \item{\code{fisher.test}:}{
      Object of class \code{"matrix"} containing a Fisher test giving
      the significance of fidelity values.
    }
    \item{\code{lowerCI}:}{
      Object of class \code{"matrix"} containing boot strap confidence
      intervals. All values are \code{NA} if no bootstrap was performed
      by a call to \code{\link{Fidelity}}.
    }
    \item{\code{upperCI}:}{
      Same as \code{lowerCI}
    }
    \item{\code{nboot}:}{
      Object of class \code{"integer"} giving the number of boot strap
      replicates performed. See above.
    }
    \item{\code{method}:}{
      Object of class \code{"character"} giving the name of the method.
    }
  }
}

\section{Extends}{
Class \code{"\linkS4class{VegsoupPartition}"}, directly.
Class \code{"\linkS4class{Vegsoup}"}, by class "VegsoupPartition", distance 2.
}

\section{Methods}{
  These generic methods below apply to objects inheriting
  \code{'VegsoupPartitionFidelity'}. All other methods defined for classes
  \code{\linkS4class{VegsoupPartition}} or
  \code{\linkS4class{Vegsoup}} apply also by direct inheritance!
  See \sQuote{Extends}.
  \describe{
    \item{ getStat }{
      Method to return \code{slot(obj, "stat")}
    }
    \item{ summary }{
      Summary method.
    }
  }
}

\author{ Roland Kaiser }

\seealso{
\code{\link{Fidelity}}, \code{\linkS4class{VegsoupOptimstride}}
}

\examples{
require(vegsoup)

showClass("VegsoupPartitionFidelity")

data(barmstein)

x <- Fidelity(VegsoupPartition(barmstein, k = 2), method = "IndVal")

\dontrun{
# needs package parallel
if (require(parallel)) {
data(sim100)
x <- Fidelity(VegsoupPartition(sim100, k = 10), method = "IndVal", fast = TRUE)
}
}
}

\keyword{ classes }

