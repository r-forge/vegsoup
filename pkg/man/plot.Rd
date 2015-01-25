\encoding{UTF-8}

\name{plot-methods}
\docType{methods}

\alias{plot-methods}

\alias{plot,Vegsoup,missing-method}
\alias{plot,VegsoupPartition,missing-method}
\alias{plot,VegsoupOptimstride,missing-method}


\title{ Plotting methods }

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
	 Argument \code{'oc.threshold'} is passed to \code{summary(obj)} and controls the
	 threshold value of OptimClass2. \code{which} can be used to select a subset of
	 computed strides to plot.
	}
  }
}

\keyword{ methods }

