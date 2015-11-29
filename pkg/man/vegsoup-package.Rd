% markup conventions
% classes: \code{"class"} or \code{\linkS4class{Vegsoup}}
% methods and functions: \code{function()}
% arguments: \code{argument}
% objects: \code{'objects'}
% slots: \emph{slot 'objects'}
% \R for the R system itself
%aAll of ‘% { } \’ should be escaped in LaTeX-like text
% tabular width________________________ \tab \cr

\encoding{UTF-8}

\name{vegsoup-package}
\docType{package}

\alias{vegsoup-package}
\alias{vegsoup}

\title{ An overview of the functions in this package }

\description{
The vegsoup package provides classes and methods to store and manipulate vegetation data (spatial and temporal explicit species co-occurrence data). In a certain respect it acts like a data base system. Vegetation data consists of at least of two related tables -- a species or community matrix and a table of measurements taken at a site (sampling unit or \enc{relevée}{relevee}). If taxonomic identity or traits of species is a concern, an additional table has to be tightly linked to the species matrix. Moreover, if spatial representation (e.g. plot coordinates and/or polygons) needs to be addressed another data layer has to be included. In the most complete case, up to five independent \R objects have to be managed. Indexing and manipulation easily get cumbersome in this situation and the general workflow is hindered by a serious amount of code that ensures linking of objects.

This package should be useful in the field of vegetation ecology, vegetation mapping and classification, and phytosociology in particular.

}

\details{

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
\code{"Vegsoup"} objects or their foundation classes (e.g. \code{\linkS4class{Species}}, \code{\linkS4class{Sites}}, \code{\linkS4class{Taxonomy}}) can be created (1) from file
connections including data base views, (2) from local \R objects, (3) from community
simulations, (4) from plain text files (OCR-scans) or (5) from other classes,
with the following functions:

 \tabular{ll}{
  \code{\link{Vegsoup}} \tab
    main constructor function demanding full specification \cr
  \code{\link{plainsoup}} \tab
    supply just a community matrix and no further data elements \cr
  \code{\link{instantsoup}} \tab
    shorthand function when data files conform to some format \cr
  \code{\link{coenoflex}} \tab
     random community of specified size \cr
  \code{\link{species}} \tab
    constructor function for class \code{"Species"} \cr
  \code{\link{sites}} \tab
    constructor function for class \code{"Sites"} \cr
  \code{\link{taxonomy}} \tab
    constructor function for class \code{"Taxonomy"} \cr
  \code{\link{stackSpecies}} \tab
    reshape a vegetation table from wide to long format \cr
  \code{\link{stackSites}} \tab
    as above, but for sites data \cr
  \code{\link{reshapeSpecies}} \tab
    conversion function to class \code{"Species"} \cr
  \code{\link{stackCoordinates}} \tab
    collects spatial coordinates \cr
  \code{\link{read.verbatim}} \tab
    reads data from text arrays (OCR scans) \cr
  ________________________ \tab \cr
 }
}

\section{ Derived objects from base class Vegsoup }{
The \pkg{vegsoup} package uses S4 classes that relate to each other by direct inheritance. There are 2 derived class from the base class \code{"Vegsoup"}. Objects of class \code{"VegsoupPartition"} directly extend the former class, that means all methods defined for class \code{"Vegsoup"} apply. As the name suggest, class \code{"VegsoupPartition"} represents an object that is \emph{partitioned} (clusterend).

The following functions and method are designed for class \code{"VegsoupPartition"}.

 \tabular{ll}{
  \code{\link{VegsoupPartition}} \tab
    constructor function \cr
  \code{\link{stable}} \tab
    classification stability \cr
  ________________________ \tab \cr
  }

Class \code{"VegsoupOptimstride"} also extends class \code{"Vegsoup"}. Thjis class contains information on how a data set could be partitioned.

 \tabular{ll}{
  \code{\link{OptimStride}} \tab
    compute a series of clusterings by different methods and quantify their quality \cr
  \code{\link{stride}} \tab
    number of faithful species for each partition \cr
  \code{\link{threshold}} \tab
    threshold values set for the Fisher test \cr
  \code{\link{optimclass1},\link{optimclass2}} \tab
    matrices with the counts of faithful species \cr
  \code{\link{peaks}\link{which.max}} \tab
    peaks along the stride and \cr
  \code{\link{which.max}} \tab
    number of k that coincides with the highest number of faithful species \cr
  ________________________ \tab \cr
 }
 
When indicator species analysis was performed on an object of that class, an object of \code{"VegsoupPartitionFidelity"} obtained, again with it's own unique and additional properties. 

}

\section{ Properties of Vegsoup* objects }{

As outlined above, a \code{"Vegsoup"} object contains several data partitions. Besides the species and sites data it contains a taxonomic reference for all taxa, a definition of a cover scale, some standardisation statement(s), a dissimilarity measure to apply, and spatial coordinates. These properties can be queried (or changed) using a set of methods.

 \tabular{ll}{
  \code{\link{decostand}} \tab
    retrieve or set a standardization method \cr
  \code{\link{vegdist}} \tab
    specify a dissimilarity measures \cr
  \code{\link{nrow},\link{ncol},\link{dim}} \tab
    return the dimensions of the species matrix \cr
  \code{\link{ncell},\link{fill}} \tab
    report the number of cells and the proportion non zero elements in the species matrix \cr
  \code{\link{coverscale}} \tab
    get or set coverscale \cr
  \code{\link{layers}} \tab
    the unique layers/strata \cr
  \code{\link{extent}\link{bbox}} \tab
    spatial extent \cr
  \code{\link{proj4string}} \tab
    projection attributes (coordinate reference system) \cr
  ________________________ \tab \cr
 }

For \code{"VegsoupPartition"} objects the following additional methods are defined.

 \tabular{ll}{
  \code{\link{partitioning}} \tab
    partitioning vector \cr
  \code{\link{getK}} \tab
    number of partitions \cr
  ________________________ \tab \cr
 }

}

\section{ Summarizing and validation }{

Summary and print method

 \tabular{ll}{
   \code{\link{print}} \tab
     show summary \cr
   \code{\link{head},\link{tail}} \tab
      return the first or last parts of the site data frame \cr
   \code{\link{richness}} \tab
     dataset, sample or pooled species richness \cr
  ________________________ \tab \cr
 }

For \code{"VegsoupPartition"} objects the following additional methods are defined.

 \tabular{ll}{
   \code{\link{spread}} \tab
    lists all occurrences of species in partitions \cr
   \code{\link{shared}} \tab
    matrix showing percentages of shared species between partitons \cr
   \code{\link{typical}} \tab
    identifies plots that typify partitions \cr
   \code{\link{outlier}} \tab
    outliers based on mean inter-plot dissimilarity or nearest neighbour criterion \cr
   \code{\link{silhouette}} \tab
    evaluates cluster by silhouette information \cr
   \code{\link{partana}} \tab
    evaluates cluster by partition analysis \cr
   \code{\link{disdiam}} \tab
    evaluates by dissimilarity diameters \cr
   \code{\link{tabdev}} \tab
    calculates the probability of observing as low a deviance as observed \cr
   \code{\link{singleton}} \tab
    test if any partition has only 1 member \cr
  ________________________ \tab \cr
 }
}

\section{ The species matrix stored in Vegsoup objects }{
There exist several methods that retrieve a community table as class 'matrix' from \code{"Vegsoup"} objects. All methods access \emph{slot 'species'} of an object. Transposed versions of each are available with the mode argument.

 \tabular{ll}{
  \code{\link{as.logical}} \tab
    returns a \code{"matrix"} with zeros coding for absences and ones for presences\cr
  \code{\link{as.numeric}} \tab
    returns a \code{"matrix"} with species abundances decoded by a coverscale \cr
  \code{\link{as.character}} \tab
    returns a \code{"matrix"} with species abundances in the data set's original scale \cr
  \code{\link{as.matrix}} \tab
    equivalent to the above, but storage type controlled by argument \code{typeof} \cr
  \code{\link{as.array}} \tab
    multidimensional dimensional array where is layer represents a dimension \cr
  \code{\link{as.vector}} \tab
    returns a vector with storage mode controlled by argument \code{mode} \cr
  \code{\link{indices}} \tab
    pointer to non empty cells in the species matrix \cr
  \code{\link{rowSums}\link{colSums}} \tab
    row and column sums for the the species matrix \cr
  \code{\link{rowMeans}\link{colMeans}} \tab
    row and column means for the the species matrix \cr
  \code{\link{as.dist}} \tab
    compute a dissimilarity matrix \cr
  ________________________ \tab \cr
 }

For \code{"VegsoupPartition"} objects the following additional methods are defined. 

 \tabular{ll}{
  \code{\link{quantile}} \tab
    sample quantiles for each species in each partition \cr
  ________________________ \tab \cr
 }
}

\section{ Row and Column Names }{

Vegsoup objects have row and column names. Row names are the plots (releveé) and they are shared among the species matrix and the sites data frame. Column names are therefore either dimnames of the species matrix (class \code{matrix}) or the names of the sites data (\code{colnames} relates to the species matrix, whereas \code{names} relates to the sites data). The \code{"\$"} operator acts like with normal data frames, it extracts the sites data. Replacement methods are also defined.

 \tabular{ll}{
  \code{\link{rownames},\link{row.names}} \tab
    plot names (mode \code{"character"}), the identifier of the sampling units \cr
  \code{\link{colnames}} \tab
    compound string of abbreviated scientific species names and layer (collapsed with @) \cr
  \code{\link{dimnames}} \tab
    \code{dimnames(as.matrix(x))} equaling rownames and colnames as above \cr
  \code{\link{names}} \tab
    column names of the sites data \cr
  ________________________ \tab \cr
 }
}

\section{ Print and summary methods }{

There are several ways to manipulate vegsoup objects.

 \tabular{ll}{
  \code{\link{relevee}} \tab
    list a single relevee \cr
  \code{\link{taxalist}} \tab
    list of taxon names \cr
  \code{\link{abbr}} \tab
    abbreviations (abbr) used to encode scientific species names \cr
  ________________________ \tab \cr
 }
}

\section{ Manipulating Vegsoup objects }{

There are several ways to manipulate \code{"Vegsoup*"} objects.

 \tabular{ll}{
  \code{\link{bind}} \tab
    combine a sequence of objects and to a single object \cr
  \code{\link{Extract}} \tab
    basic row and column-wise indexing \cr
  \code{\link{subset}} \tab
    subset based on literal taxon names \cr
  \code{\link{combine}} \tab
    combines two or more taxa by selecting scientific species names \cr
  \code{\link{combine}} \tab
    combines two or more taxa by selecting scientific species names \cr
  \code{\link{layers<-}} \tab
    modify an object's layer structure \cr
  ________________________ \tab \cr
 }

For \code{"VegsoupPartition"} objects the following additional methods are defined.

 \tabular{ll}{
  \code{\link{partition}} \tab
    extract parts (partitions) of an object \cr
  \code{\link{reorder}} \tab
    reorder an partitioned object based on the topology of a dendrogram \cr
  \code{\link{seriation}} \tab
     modify an objects internal order as to reflect a vegetation table \cr
  ________________________ \tab \cr
 }
}

\section{ Sample Operation }{

Nothing here yet.
 \tabular{ll}{
  \code{\link{sample}} \tab
    random aamples and permutations \cr
  \code{\link{hcr}} \tab
    heterogeneity-constrained random samples \cr
  ________________________ \tab \cr
 }
}

\section{ Table methods }{

Only class \code{"VegsoupPartition"} has meaningful table methods.

 \tabular{ll}{
   \code{\link{constancy}} \tab
     constancy table \cr
   \code{\link{contingency}} \tab
     contingency table \cr
   \code{\link{average}} \tab
     table of mean abundances in partitions \cr
   \code{\link{confusion}\link{accuracy}} \tab
     cross-classifies each partition by actual and predicted class memberships \cr
   \code{\link{as.table}} \tab
     tabulation of partitioning(obj) against a cross-classifying factor \cr
   \code{\link{partitioningMatrix}} \tab
     memberships of plots to partitions \cr
   \code{\link{partitioningCombinations}} \tab
     tabulate all possibele combinations of partitions \cr
  ________________________ \tab \cr
 }
}

\section{ Optimization }{
 \tabular{ll}{
  \code{\link{optsil}} \tab
    modify partitioning as to optimize silhouette widths \cr
  \code{\link{optindval}} \tab
    modify partitioning as to optimize Dufrene and Legendre's indicator value \cr
  ________________________ \tab \cr
 }
}

\section{ Spatial methods }{

Methods from spatial packges.

 \tabular{ll}{
 \code{\link{spTransform}} \tab
   extract the values of a \code{"raster"} object \cr
 \code{\link{over}} \tab
   overlay method for spatial polygons \cr
 \code{\link{spTransform}} \tab
   extract the values of a \code{"raster"} object \cr
  \code{\link{extract}} \tab
   extract the values of a \code{"raster"} object \cr
  ________________________ \tab \cr
 }
}

\section{ Plotting }{
Plot
 \tabular{ll}{
  \code{\link{quicklook}} \tab
    opens a browser window and plots pushpins of plot coordiantes \cr
%  \code{\link{ellipsoidhull}} \tab
%    computes the ellipsoid hull or spanning ellipsoid \cr
  \code{\link{rectangles}} \tab
    plots a box according to number of species and sites for each partition \cr
  \code{\link{plotPCO}} \tab
    plot principal coordinates \cr
  \code{\link{coldiss}} \tab
    plots unordered and ordered colored matrix of dissimilarities \cr
  ________________________ \tab \cr
 }
}

\section{ Typesetting }{

 \tabular{ll}{
  \code{\link{Latex}} \tab \cr
  ________________________ \tab \cr
 }
}

\section{ Writing files }{

Basic and advanced writing of entire files or parts thereof.

 \tabular{ll}{
  \code{\link{write.verbatim}} \tab
    generates a table structures with mono-spaced fonts \cr
  \code{\link{Latex}} \tab
    table structures to be compiled by LaTeX \cr
  \code{\link{KML}} \tab
    visualization in google earth \cr
  ________________________ \tab \cr
 }
}

\section{ Miscellaneous }{
Miscellaneous methods and functions
 \tabular{ll}{
  \code{\link{compass}} \tab
    cuts numeric values of angular degrees into characters of the wind-rose \cr
  \code{\link{tile2latlng}} \tab
    geocoding functionality using the grid scheme of tile map services \cr
  \code{\link{elevation}} \tab
    topographic height information from the SRTM3 dat set \cr
  ________________________ \tab \cr
 }
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

