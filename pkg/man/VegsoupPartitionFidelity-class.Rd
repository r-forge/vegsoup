\encoding{UTF-8}

\name{VegsoupPartitionFidelity-class}
\Rdversion{1.1}
\docType{class}

\alias{VegsoupPartitionFidelity-class}

\title{ VegsoupPartitionFidelity Class }

\description{
A class extending class \code{\linkS4class{VegsoupPartition}}.
}

\section{ Objects from the Class }{
Objects can be created by calls to the function \code{\link{fidelity}}.

}

\section{Slots}{
  Class slots extending \verb{VegsoupPartition}
  \describe{
    \item{\code{stat}:}{
      Object of class \verb{matrix} containing the fidelity values.
     }
    \item{\code{fisher.test}:}{
      Object of class \verb{matrix} containing a Fisher test giving
      the significance of fidelity values.
    }
    \item{\code{lowerCI}:}{
      Object of class \verb{matrix} containing boot strap confidence
      intervals. All values are \code{NA} if no bootstrap was performed
      by a call to \code{\link{fidelity}}.
    }
    \item{\code{upperCI}:}{
      Same as \code{lowerCI}
    }
    \item{\code{nboot}:}{
      Object of class \verb{integer} giving the number of bootstrap
      replicates performed. See above.
    }
    \item{\code{fidelity.method}:}{
      Object of class \verb{character} giving the name of the method.
    }
  }
}

\section{Extends}{
Class \code{\linkS4class{VegsoupPartition}}, directly.
Class \code{\linkS4class{Vegsoup}}, by class \verb{VegsoupPartition}, distance 2.
}

\section{Methods}{
  These generic methods below apply to objects inheriting
  \verb{VegsoupPartitionFidelity}. All other methods defined for classes
  \code{\linkS4class{VegsoupPartition}} or
  \code{\linkS4class{Vegsoup}} apply also by direct inheritance
  (See \sQuote{Extends}).
  \describe{
    \item{ getStat }{
      Method to return \emph{slot(obj, 'stat')}
    }
    \item{ summary }{
      Summary method
    }
  }
}

\author{ Roland Kaiser }

\seealso{
\code{\link{fidelity}},
\code{\linkS4class{VegsoupOptimstride}}
}

\examples{
require(parallel)

showClass("VegsoupPartitionFidelity")

data(barmstein)

x <- fidelity(VegsoupPartition(barmstein, k = 2),
              method = "IndVal")
x

x <- fidelity(VegsoupPartition(coenoflex(1000, 50), k = 10),
              method = "IndVal", fast = TRUE)
x
}
}

\keyword{ classes }

