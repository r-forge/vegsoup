\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,Vegsoup,missing-method}
\alias{plot,Vegsoup,ANY-method}
\alias{plot,VegsoupPartition,missing-method}
\alias{plot,VegsoupOptimstride,ANY-method}

\title{ Plotting methods for \code{Vegsoup*} Objects }
\description{
Methods for function \code{plot}.
}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "Vegsoup", y = "missing")}}{
      Plot two histograms.
    }

    \item{\code{signature(x = "VegsoupPartition", y = "missing")}}{
      Simple ordination plot.
    }
    \item{\code{signature(x = "VegsoupOptimstride", y = "missing")}}{
     Argument \code{'oc.treshold'} is passed to summary(obj) and controls the
     threshold value of OptimClass2 (see Tichy 2010)
    }
  }
}
\keyword{ methods }
