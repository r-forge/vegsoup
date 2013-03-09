\encoding{UTF-8}
\name{vegsoup-package}
\alias{vegsoup-package}
\alias{vegsoup}
\docType{package}
\title{ An overview of the functions in this package }
\description{
The vegsoup package provides classes and functions to manipulate vegetation data. It can be seen as a data base system. Vegetation data consists at least of two related tables -- a species matrix and a table of sites properties, including measured environmental varibales. If taxonomic idenity, or traits of species is a concern an additional table has to be tightly linked the species matrix. Moreover, if spatial representation (e.g. plot coordinates and/or polygons) needs to be adressed another data layer has to be included. In the most complete case we end up with indipendent R objects. Indexing and manipulation easily get cumbersome in this situation and the general workflow is hindered by a serious amount code that ensures linking of objects 

The package should be particularly usefull when \dots.

Below is a list of functions grouped by theme.
}
\details{

The package implements classes for vegetation data (see \link{Vegsoup-class}) and supports
  \itemize{
    \item data to be structured in layers/strata.
    \item several cover scales and conversions thereof.
    \item taxonomic treatment including species traits.
    \item standardizations and dissimilarity measures of the species data.
    \item creation of species and dissimilarity/distance matrices.
    \item indexing, extract, replace and sample operations.
    \item row and column aggregation
    \item merging of objects
    \item coerce methods
    \item output methods
    \item spatial methods
    \item plotting methods
  }
}

\section{I. Creating Vegsoup* objects}{
Vegsoup* objects can be created, from files, from data base views, from objects, from community simualtions or from other classes, with the following functions:
  \tabular{ll}{
   \code{\link{}} \tab \cr
  }
RasterLayer, RasterStack, and RasterBrick objects are, as a group, referred to as Raster* objects
}

%% 1
\section{I. Manipulating of Vegsoup* objects}{
\tabular{ll}{
 \code{\link{}} \tab \cr
}
}

\section{I. Set or changing the coverscale of Vegsoup* objects}{
Coverscale
\tabular{ll}{
 \code{\link{}} \tab \cr
}
}

\section{I. Summarizing}{
Summary
\tabular{ll}{
 \code{\link{}} \tab \cr
}
}

\section{I. Accessing values of Vegsoup* objects}{
\tabular{ll}{
 \code{\link{}} \tab \cr
}
}

\section{I. Plotting}{
Plot
\tabular{ll}{
 \code{\link{}} \tab \cr
}
}

\section{I. Typesetting}{
Latex
\tabular{ll}{
 \code{\link{}} \tab \cr
}
}

\section{I. Getting and setting Vegsoup* dimensions }{
\tabular{ll}{
 \code{\link{}} \tab \cr
}
}

\section{I. Writing files}{
Basic writing
\tabular{ll}{
 \code{\link{}} \tab \cr
}
Advanced writing of entire files or parts thereof
  \tabular{ll}{
   \code{\link{}} \tab \cr
  }
}

\section{I. Miscellaneous}{
  \tabular{ll}{
   \code{\link{}} \tab \cr
  }
}
\author{
Roland Kaiser

Maintainer: Roland Kaiser \email{kardinal.eros@gmail.com}

}
\section{Acknowledgements}{
Extensive suggestions were made by \dots and many others.
}

\keyword{ package }
\keyword{ multivariate }
\keyword{ manip }
\keyword{ spatial }


\seealso{
\code{\link[vegdata:vegdata-package]{vegdata}}
\code{\link[mefa:mefa-package]{mefa}}
\code{\link[multitable:multitable-package]{multitable}}
\code{\link[vegan:vegan-package]{vegan}}
\code{\link[optpart:optpart-package]{optpart}}
\code{\link[labdsv:labdsv-package]{labdsv}}
}
