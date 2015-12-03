% markup conventions
% arguments{} section: mode and/or type of argument terminated by a dot
% (e.g. \verb{character}. Any method ...)
% classes: \verb{class} or \code{\linkS4class{Vegsoup}}
% methods and functions: \code{function()}
% arguments: \code{argument}
% objects: \code{'objects'}
% slots: \emph{slot 'objects'}
% \R for the R system itself
%aAll of ‘% { } \’ should be escaped in LaTeX-like text
% tabular width________________________ }{

\encoding{UTF-8}

\name{vegsoup-package}
\docType{package}

\alias{vegsoup-package}
\alias{vegsoup}

\title{ An overview of the functions in this package }

\description{
The vegsoup package provides classes and methods to store and manipulate vegetation data (spatial and temporal explicit species co-occurrence data). In a certain respect it acts like a data base system. In fact, it's data model is very similar to the \emph{TurboVeg} XML interchange format (see \code{\link{read.XML}}).

Vegetation data consists of at least of two related tables -- a species or community matrix and a table of measurements taken at a site (sampling unit or \enc{relevée}{relevee}). At the very least a sampling date is available. If taxonomic identity or traits of species is a concern -- and it should be --  an additional table has to be tightly linked to the species matrix. Moreover, if spatial representation (e.g. plot coordinates and/or polygons) needs to be addressed another data layer has to be included. In the most complete case, up to five independent \R objects have to be managed. Indexing and manipulation easily get cumbersome in this situation and the general workflow is hindered by a serious amount of code that ensures linking of objects, for example when random subsets needs be performed. Some \R package adress these issues, namely the \pkg{mefa} and \pkg{multitable} packages. They do so by extending the \code{data.frame} concept. None of these packages is spatially explicit nor handle cover scales commonly applied in vegetation ecology and present vegetation data bases.

Besides that the package provides novel functionality, it also interfaces functions in other packages, most important are \pkg{vegan}, \pkg{optpart} and \pkg{labdsv}. These functions are extended or their arguments are simplified, due the inherent properties og \verb{Vegsoup} objects. It was avoided to create new function names whenever another package already defines a suitable and meaningful name. Examples are \code{decostand()} and \code{vegdist()} which have extended meaning.

The \emph{vegsoup} term follows the tradition of strange named functions in the \pkg{vegan} package – in particular \code{\link[vegan:vegemite]{vegemite}}, to give but one example but also notice the \code{tabasco() function}.

This package should be mainly useful in the field of vegetation ecology, vegetation mapping and classification, and phytosociology in particular.

}

\section{ Summary }{

The package implements classes for vegetation data (see \link{Vegsoup-class}) and supports

\itemize{
  \item a solid class that is capable to store all relevant information
  \item species data to be structured in layers/strata
  \item several cover scales and conversions thereof
  \item taxonomic treatment (possibly including species traits) and amendment
  \item selection of standardizations
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

Type \code{vignette("vegsoup")} for some usage example.

}

\section{ Creating Vegsoup objects and their base classes }{

\verb{Vegsoup} objects or their foundation classes (e.g.
\code{\linkS4class{Species}}, \code{\linkS4class{Sites}},
\code{\linkS4class{Taxonomy}}) can be created (1) from file
connections including data base views, (2) from local \R objects, (3) from
community simulations, (4) from plain text files (OCR-scans) or (5) from other
classes, with the following functions:

 \describe{
  \item{ \code{\link{Vegsoup}} }{
    main constructor function demanding full specification
  }
  \item{ \code{\link{plainsoup}} }{
    suply just a community matrix and coverscale but no further data elements
  }
  \item{ \code{\link{instantsoup}} }{
    shorthand function when data files conform to specific conventions (format)
  }
  \item{ \code{\link{coenoflex}} }{
     random community of specified size
  }
  \item{ \code{\link{species}} }{
    constructor function for class \verb{Species}
  }
  \item{ \code{\link{sites}} }{
    constructor function for class \verb{Sites}
  }
  \item{ \code{\link{taxonomy}} }{
    constructor function for class \verb{Taxonomy}
  }
  \item{ \code{\link{stackSpecies}} }{
    reshape a vegetation table from wide to long format
  }
  \item{ \code{\link{stackSites}} }{
    as above, but for sites data
  }
  \item{ \code{\link{reshapeSpecies}} }{
    conversion function to class \verb{Species}
  }
  \item{ \code{\link{stackCoordinates}} }{
    collects spatial coordinates
  }
  \item{ \code{\link{read.verbatim}} }{
    reads data from text arrays (OCR scans)
  }
  \item{ \code{\link{read.XML}} }{
    reads data from XML files exported from the turboveg data base
  }
 }

}

\section{ Derived objects from base class Vegsoup }{
The \pkg{vegsoup} package uses S4 classes that relate to each other by direct inheritance. There are 2 derived class from the base class \verb{Vegsoup}. Objects of class \verb{VegsoupPartition} directly extend the former class, that means all methods defined for class \verb{Vegsoup} apply. As the name suggest, class \verb{VegsoupPartition} represents an object that is \emph{partitioned} (clusterend).

The following functions and method are designed for class \verb{VegsoupPartition}.

 \describe{
  \item{ \code{\link{VegsoupPartition}} }{
    constructor function
  }
  \item{ \code{\link{stable}} }{
    classification stability
  }
 }

Class \verb{VegsoupOptimstride} also extends class \verb{Vegsoup}. Thjis class contains information on how a data set could be partitioned.

 \describe{
  \item{ \code{\link{OptimStride}} }{
    compute a series of clusterings by different methods and quantify their quality
  }
  \item{ \code{\link{stride}} }{
    number of faithful species for each partition
  }
  \item{ \code{\link{threshold}} }{
    threshold values set for the Fisher test
  }
  \item{ \code{\link{optimclass1},\link{optimclass2}} }{
    matrices with the counts of faithful species
  }
  \item{ \code{\link{peaks}\link{which.max}} }{
    peaks along the stride and
  }
  \item{ \code{\link{which.max}} }{
    number of k that coincides with the highest number of faithful species
  }
 }

When indicator species analysis was performed on an object of that class, an object of \verb{VegsoupPartitionFidelity} obtained, again with it's own unique and additional properties. 

}

\section{ Properties of Vegsoup* objects }{

As outlined above, a \verb{Vegsoup} object contains several data partitions. Besides the species and sites data it contains a taxonomic reference for all taxa, a definition of a cover scale, some standardisation statement(s), a dissimilarity measure to apply, and spatial coordinates. Basic parameters of existing \verb{Vegsoup} objects can be obtained, and in most cases changed.

 \describe{
  \item{ \code{\link{decostand}} }{
    retrieve or set a standardization method
  }
  \item{ \code{\link{vegdist}} }{
    specify a dissimilarity measures
  }
  \item{ \code{\link{nrow},\link{ncol},\link{dim}} }{
    return the dimensions of the species matrix
  }
  \item{ \code{\link{ncell},\link{fill}} }{
    report the number of cells and the proportion non zero elements in the
    species matrix
  }
  \item{ \code{\link{coverscale}} }{
    get or set coverscale
  }
  \item{ \code{\link{layers}} }{
    the unique layers/strata
  }
  \item{ \code{\link{extent}\link{bbox}} }{
    spatial extent
  }
  \item{ \code{\link{proj4string}} }{
    projection attributes (coordinate reference system)
  }
 }

For \verb{VegsoupPartition} objects the following additional methods are defined.

 \describe{
  \item{ \code{\link{partitioning}} }{
    partitioning vector
  }
  \item{ \code{\link{getK}} }{
    number of partitions
  }
 }

}

\section{ Summarizing and validation }{

Summary and print method

 \describe{
  \item{ \code{\link{print}} }{
     show summary
  }
  \item{ \code{\link{head},\link{tail}} }{
      return the first or last parts of the site data frame
  }
  \item{ \code{\link{richness}} }{
     dataset, sample or pooled species richness
  }
 }

For \verb{VegsoupPartition} objects the following additional methods are defined.

 \describe{
  \item{ \code{\link{spread}} }{
    lists all occurrences of species in partitions
  }
  \item{ \code{\link{shared}} }{
    matrix showing percentages of shared species between partitons
  }
  \item{ \code{\link{typical}} }{
    identifies plots that typify partitions
  }
  \item{ \code{\link{outlier}} }{
    outliers based on mean inter-plot dissimilarity or nearest neighbour criterion
  }
  \item{ \code{\link{silhouette}} }{
    evaluates cluster by silhouette information
  }
  \item{ \code{\link{partana}} }{
    evaluates cluster by partition analysis
  }
  \item{ \code{\link{disdiam}} }{
    evaluates by dissimilarity diameters
  }
  \item{ \code{\link{tabdev}} }{
    calculates the probability of observing as low a deviance as observed
  }
  \item{ \code{\link{singleton}} }{
    test if any partition has only 1 member
  }
 }

}

\section{ The species matrix stored in Vegsoup objects }{
There exist several methods that retrieve a community table as class 'matrix' from \verb{Vegsoup} objects. All methods access \emph{slot 'species'} of an object. Transposed versions of each are available with the mode argument.

 \describe{
  \item{ \code{\link{as.logical}} }{
    returns a \verb{matrix} with zeros coding for absences and ones for presences
  }
  \item{ \code{\link{as.numeric}} }{
    returns a \verb{matrix} with species abundances decoded by a coverscale
  }
  \item{ \code{\link{as.character}} }{
    returns a \verb{matrix} with species abundances in the data set's original scale
  }
  \item{ \code{\link{as.matrix}} }{
    equivalent to the above, but storage type controlled by argument \code{typeof}
  }
  \item{ \code{\link{as.array}} }{
    multidimensional dimensional array where is layer represents a dimension
  }
  \item{ \code{\link{as.vector}} }{
    returns a vector with storage mode controlled by argument \code{mode}
  }
  \item{ \code{\link{indices}} }{
    pointer to non empty cells in the species matrix
  }
  \item{ \code{\link{rowSums}\link{colSums}} }{
    row and column sums for the the species matrix
  }
  \item{ \code{\link{rowMeans}\link{colMeans}} }{
    row and column means for the the species matrix
  }
  \item{ \code{\link{as.dist}} }{
    compute a dissimilarity matrix
  }
 }

For \verb{VegsoupPartition} objects the following additional methods are defined. 

 \describe{
  \item{ \code{\link{quantile}} }{
    sample quantiles for each species in each partition
  }
 }

}

\section{ Row and Column Names }{

Vegsoup objects have row and column names. Row names are the plots (releveé) and they are shared among the species matrix and the sites data frame. Column names are therefore either dimnames of the species matrix (class \code{matrix}) or the names of the sites data (\code{colnames} relates to the species matrix, whereas \code{names} relates to the sites data). The \verb{$} operator acts like with normal data frames, it extracts the sites data. Replacement methods are also defined.

 \describe{
  \item{ \code{\link{rownames},\link{row.names}} }{
    plot names (mode \verb{character}), the identifier of the sampling units
  }
  \item{ \code{\link{colnames}} }{
    compound string of abbreviated scientific species names and layer (collapsed with @)
  }
  \item{ \code{\link{dimnames}} }{
    \code{dimnames(as.matrix(x))} equaling rownames and colnames as above
  }
  \item{ \code{\link{names}} }{
    column names of the sites data
  }
 }

}

\section{ Print and summary methods }{

There are several ways to manipulate vegsoup objects.

 \describe{
  \item{ \code{\link{relevee}} }{
    list a single relevee
  }
  \item{ \code{\link{taxalist}} }{
    list of taxon names
  }
  \item{ \code{\link{abbr}} }{
    abbreviations (abbr) used to encode scientific species names
  }
 }

}

\section{ Manipulating Vegsoup objects }{

There are several ways to manipulate \verb{Vegsoup*} objects.

 \describe{
  \item{ \code{\link{bind}} }{
    combine a sequence of objects and to a single object
  }
  \item{ \code{\link{Extract}} }{
    basic row and column-wise indexing
  }
  \item{ \code{\link{subset}} }{
    subset based on literal taxon names
  }
  \item{ \code{\link{combine}} }{
    combines two or more taxa by selecting scientific species names
  }
  \item{ \code{\link{combine}} }{
    combines two or more taxa by selecting scientific species names
  }
  \item{ \code{\link{layers<-}} }{
    modify an object's layer structure
  }
 }

For \verb{VegsoupPartition} objects the following additional methods are defined.

 \describe{
  \item{ \code{\link{partition}} }{
    extract parts (partitions) of an object
  }
  \item{ \code{\link{reorder}} }{
    reorder an partitioned object based on the topology of a dendrogram
  }
  \item{ \code{\link{seriation}} }{
     modify an objects internal order as to reflect a vegetation table
  }
 }

}

\section{ Sample Operation }{

Nothing here yet.
 \describe{
  \item{ \code{\link{sample}} }{
    random aamples and permutations
  }
  \item{ \code{\link{hcr}} }{
    heterogeneity-constrained random samples
  }
 }

}

\section{ Table methods }{

Only class \verb{VegsoupPartition} has meaningful table methods.

 \describe{
  \item{ \code{\link{constancy}} }{
     constancy table
  }
  \item{ \code{\link{contingency}} }{
     contingency table
  }
  \item{ \code{\link{average}} }{
     table of mean abundances in partitions
  }
  \item{ \code{\link{confusion}\link{accuracy}} }{
     cross-classifies each partition by actual and predicted class memberships
  }
  \item{ \code{\link{as.table}} }{
     tabulation of partitioning(obj) against a cross-classifying factor
  }
  \item{ \code{\link{partitioningMatrix}} }{
     memberships of plots to partitions
  }
  \item{ \code{\link{partitioningCombinations}} }{
     tabulate all possibele combinations of partitions
  }
 }

}

\section{ Optimization }{

The clustering partition vetcor of \verb{VegsoupPartition} objects can be optimized.
 \describe{
  \item{ \code{\link{optsil}} }{
    modify partitioning as to optimize silhouette widths
  }
  \item{ \code{\link{optindval}} }{
    modify partitioning as to optimize Dufrene and Legendre's indicator value
  }
 }

}

\section{ Spatial methods }{

Methods from spatial packges.

 \describe{
  \item{ \code{\link{spTransform}} }{
   extract the values of a \verb{raster} object (package \pkg{sp})
  }
  \item{ \code{\link{over}} }{
   overlay method for spatial polygons (package \pkg{sp})
  }
  \item{ \code{\link{extract}} }{
   extract the values of a \verb{raster} object (package \pkg{raster})
  }
 }

}

\section{ Plotting }{

 \describe{
  \item{ \code{\link{quicklook}} }{
    opens a browser window and plots pushpins of plot coordiantes
  }
  \item{ \code{\link{rectangles}} }{his
    plots a box according to number of species and sites for each partition
  }
  \item{ \code{\link{plotPCO}} }{
    plot principal coordinates
  }
  \item{ \code{\link{coldiss}} }{
    plots unordered and ordered colored matrix of dissimilarities
  }
 }

}

\section{ Typesetting }{

 \describe{
  \item{ \code{\link{Latex}} }{
    Create synoptic vegetation tables to be compiled by with LaTex.
    The \pkg{synoptic} package (\url{https://github.com/kardinal-eros/synoptic})
    provides another implementation.
  }
 }

}

\section{ Writing files }{

Basic and advanced writing of entire files or parts thereof.

 \describe{
  \item{ \code{\link{write.verbatim}} }{
    generates a table structures with mono-spaced fonts
  }
  \item{ \code{\link{Latex}} }{
    table structures to be compiled by LaTeX
  }
  \item{ \code{\link{KML}} }{
    visualization in google earth
  }
 }

}

\section{ Miscellaneous }{

Miscellaneous methods and functions.

 \describe{
  \item{ \code{\link{compass}} }{
    cuts numeric values of angular degrees into characters of the wind-rose
  }
  \item{ \code{\link{tile2latlng}} }{
    geocoding functionality using the grid scheme of tile map services
  }
  \item{ \code{\link{elevation}} }{
    topographic height information from the SRTM3 dat set
  }
 }

}

\author{
Maintainer: Roland Kaiser \email{kardinal.eros@gmail.com}
}

\section{ Acknowledgements }{
Extensive suggestions were made by \dots and many others.
}

\seealso{
\code{\link[vegan:vegan-package]{vegan}}
\code{optpart}
\code{labdsv}
% no package overview for these 2 packages
%\code{\link[optpart:optpart-package]{optpart}}
%\code{\link[labdsv:labdsv-package]{labdsv}}
\code{\link[vegdata:vegdata-package]{vegdata}}
\code{\link[mefa:mefa-package]{mefa}}
\code{\link[multitable:multitable-package]{multitable}}
}

\keyword{ package }
\keyword{ multivariate }
\keyword{ manip }
\keyword{ spatial }

