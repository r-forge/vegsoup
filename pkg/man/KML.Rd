\encoding{UTF-8}

\name{KML}
\docType{methods}

\alias{KML}
\alias{KML,Vegsoup-method}
\alias{KML,VegsoupPartition-method}

\title{ Write KML Files to Disk }

\description{
  A simple utility function for visualization of plots in google earth.
}

\usage{
\S4method{KML}{Vegsoup}(obj, file, add.label, thumbnail.url.path, website.url.path, ...)
\S4method{KML}{VegsoupPartition}(obj, file, add.label, thumbnail.url.path, website.url.path, ...)
}

\arguments{
  \item{ obj }{
   \code{'Vegsoup'} or \code{'VegsoupPartition'} object.
   }
  \item{ file }{
    path to a file, can be missing.
  }
  \item{ add.label }{
    Show labels of push-pins in KML.
  } 
  \item{ thumbnail.url.path }{
    A URL that serves a key photo graph of the plot.
    See \sQuote{Examples}.
  }
  \item{ website.url.path }{
    A URL that serves a photo gallery for a plot.
    See \sQuote{Examples}.
  }
  \item{ \dots }{
    Not used at the moment.
  }
}

\details{
  The method defined for \code{"Vegsoup"} creates pushpins that when clicked
  show a bubble with a species list for a respective plot and if
  \code{'thumbnail.url.path'} is accessible an additional key photo graph at
  the bottom of the bubble. The function creates a hyper link for that photo
  directing to a web photo gallery (\code{'website.url.path'}).

  For \code{"VegsoupPartition"} objects a KML file structured in folder is
  created. Each folder contains all plots in a respective partition. 
%%\item{\code{signature(obj = "Vegsoup")}}{
%%}
%%\item{\code{signature(obj = "VegsoupPartition")}}{
%%}
}
\author{ Roland Kaiser }

\seealso{
\code{\link{QuickMap}}
}
\examples{
require(vegsoup)

data(barmstein)

KML(barmstein, file = tempfile())
KML(VegsoupPartition(barmstein, k = 2), file = tempfile())

}
\keyword{ methods }

