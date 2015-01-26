\encoding{UTF-8}

\name{vegsoup-package}
\docType{package}

\alias{vegsoup-package}
\alias{vegsoup}

\title{ An overview of the functions in this package }

\description{
The vegsoup package provides classes and methods to store and manipulate vegetation data (spatial and temporal explicit species co-occurrence data). In one respect, it acts like a data base system. Vegetation data consists of at least of two related tables -- a species matrix and a table of measurements taken at a site (sampling unit or \enc{relevée}{relevee}). If taxonomic identity, or traits of species is a concern an additional table has to be tightly linked to the species matrix. Moreover, if spatial representation (e.g. plot coordinates and/or polygons) needs to be addressed another data layer has to be included. In the most complete case, up to five independent R objects have to be managed. Indexing and manipulation easily get cumbersome in this situation and the general workflow is hindered by a serious amount of code that ensures linking of objects.

The package should be useful in the field of vegetation ecology and phytoosociology in particular.

}

\details{

The package implements classes for vegetation data (see \link{Vegsoup-class}) and supports
\itemize{
  \item a solid class that is capable to store 
  \item species data to be structured in layers/strata
  \item several cover scales and conversions thereof
  \item taxonomic treatment (species traits.)
  \item selection of standardizations and dissimilarity
  \item ad-hoc creation of species and dissimilarity/distance matrices
  \item indexing, extract, replace and sample operations
  \item row and column aggregation
  \item merging of objects
  \item coerce methods
  \item partitioning (clustering) methods
  \item validation and refinement of partitions
  \item indicator species analysis
  \item several table methods
  \item specialized import routines
  \item output methods (Latex, text files, spatial data)
  \item spatial methods
  \item plotting and visualization methods
}

Type \code{vignette("vegsoup")} 
}

\section{ I. Creating Vegsoup objects }{
Vegsoup objects can be created, from files, from data base views, from objects, from community simulations or from other classes, with the following functions:

 \tabular{ll}{
  \code{\link{Vegsoup}} \tab \cr
  \code{\link{species}} \tab \cr
  \code{\link{sites}} \tab \cr
  \code{\link{taxonomy}} \tab \cr
  \code{\link{stackSpecies}} \tab \cr
  \code{\link{stackSites}} \tab \cr
 }
}

%% 1
\section{ I. Manipulating of Vegsoup* objects }{
Nothing here yet.
%%\tabular{ll}{
%% \code{\link{}} \tab \cr
%%}
}

\section{ I. Set or changing the coverscale of Vegsoup* objects }{

 \tabular{ll}{
  \code{\link{coverscale}} \tab \cr
  \code{\link{Coverscale}} \tab \cr
 }
}

\section{ I. Summarizing }{
Summary
%% \tabular{ll}{
%%  \code{\link{}} \tab \cr
%% }
}

\section{ I. Accessing values of Vegsoup* objects }{
Nothing here yet.
%% \tabular{ll}{
%%  \code{\link{}} \tab \cr
%% }
}

\section{ I. Plotting }{
Plot
%% \tabular{ll}{
%%  \code{\link{}} \tab \cr
%% }
}

\section{ I. Typesetting }{

 \tabular{ll}{
  \code{\link{Latex}} \tab \cr
 }
}

\section{ I. Getting and setting Vegsoup* dimensions }{
Nothing here yet.
%% \tabular{ll}{
%%  \code{\link{}} \tab \cr
%% }
}

\section{ I. Writing files }{
Basic writing
%% \tabular{ll}{
%%  \code{\link{}} \tab \cr
%% }
Advanced writing of entire files or parts thereof
%%   \tabular{ll}{
%%\code{\link{}} \tab \cr
%%   }
}

\section{ I. Miscellaneous }{
Miscellaneous methods and functions
%%   \tabular{ll}{
%%\code{\link{}} \tab \cr
%%   }
}

\author{
Maintainer: Roland Kaiser \email{kardinal.eros@gmail.com}
}

\section{ Acknowledgements }{
Extensive suggestions were made by \dots and many others.
}

\seealso{
\code{\link[vegdata:vegdata-package]{vegdata}}
\code{\link[mefa:mefa-package]{mefa}}
\code{\link[multitable:multitable-package]{multitable}}
\code{\link[vegan:vegan-package]{vegan}}
\pkg{optpart}
\pkg{labdsv}
}

\keyword{ package }
\keyword{ multivariate }
\keyword{ manip }
\keyword{ spatial }

