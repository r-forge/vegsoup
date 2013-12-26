\name{summary-methods}
\docType{methods}

\alias{summary-methods}
\alias{summary,Vegsoup-method}
\alias{summary,VegsoupPartition-method}
\alias{summary,VegsoupPartitionFidelity-method}
\alias{summary,VegsoupOptimstride-method}

\title{ Summary of Object }

\description{
  Summary Methods for \code{Vegsoup*} objects
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "Vegsoup")}}{
      If object has only one plot a species list is plotted.
    }
    \item{\code{signature(object = "VegsoupPartition")}}{
      Also print a table of partition contingencies.
    }
    \item{\code{signature(object = "VegsoupPartitionFidelity")}}{
      Prints additional information.
    }
    \item{\code{signature(object = "VegsoupOptimstride")}}{
      Prints additional information.
    }
  }
}

\author{ Roland Kaiser }

\keyword{ methods }

