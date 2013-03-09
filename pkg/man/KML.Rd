\name{KML}
\docType{methods}
\alias{KML}
\alias{KML-methods}
\title{ Write KML Files to Disk }
\description{
A simple utility function for visualization of plots in google earth.
}
\usage{
## S4 method for Vegsoup objects
KML(obj, file, thumbnail.url.path, website.url.path, ...)
## S4 method for VegsoupPartition objects
KML(obj, file, add.label = FALSE, website.url.path, thumbnail.url.path, ...)
}

\arguments{
  \item{ obj }{
   A \code{"Vegsoup"} or \code{"VegsoupPartition"} object.
   }
  \item{ file }{
    A path to a file, can be missing.
  }
  \item{ thumbnail.url.path }{
    A URL that serves a key photo graph of the plot. See \sQuote{Examples}.
  }
  \item{ website.url.path }{
    A URL that serves a photo gallery for a plot. See \sQuote{Examples}.
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
\keyword{ methods }
