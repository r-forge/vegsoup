\name{Vegsoup-class}
\Rdversion{1.1}
\docType{class}
\alias{Vegsoup-class}
\alias{[,Vegsoup,ANY,ANY,missing-method}
\alias{[<-,Vegsoup,ANY,missing-method}
\alias{$,Vegsoup-method}
\alias{$<-,Vegsoup-method}
\alias{abbr.layer,Vegsoup-method}
\alias{Abbreviation,Vegsoup-method}
\alias{AprioriGrouping,Vegsoup-method}
\alias{as.character,Vegsoup-method}
\alias{as.dist,Vegsoup-method}
\alias{as.logical,Vegsoup-method}
\alias{as.matrix,Vegsoup-method}
\alias{as.numeric,Vegsoup-method}
\alias{as.vector,Vegsoup,missing-method}
\alias{BraunBlanquetReduce,Vegsoup-method}
\alias{coerce,Vegsoup,dist-method}
\alias{coerce,Vegsoup,dsparseMatrix-method}
\alias{coerce,Vegsoup,list-method}
\alias{coerce,Vegsoup,matrix-method}
\alias{coerce,Vegsoup,sparseMatrix-method}
\alias{coldiss,Vegsoup-method}
\alias{colMeans,Vegsoup-method}
\alias{colnames,Vegsoup-method}
\alias{colSums,Vegsoup-method}
\alias{coordinates,Vegsoup-method}
\alias{coverscale,Vegsoup-method}
\alias{coverscale<-,Vegsoup,character-method}
\alias{coverscale<-,Vegsoup,Coverscale-method}
\alias{DecomposeNames,Vegsoup-method}
\alias{decostand,Vegsoup-method}
\alias{decostand<-,Vegsoup,character-method}
\alias{decostand<-,Vegsoup,NULL-method}
\alias{dim,Vegsoup-method}
\alias{dimnames,Vegsoup-method}
\alias{elevation,Vegsoup-method}
\alias{getDistconnected,Vegsoup-method}
\alias{head,Vegsoup-method}
\alias{indices,Vegsoup-method}
\alias{Indpower,Vegsoup-method}
\alias{Indspc,Vegsoup-method}
\alias{KML,Vegsoup-method}
\alias{Layer,Vegsoup-method}
\alias{Layers,Vegsoup-method}
\alias{Layers<-,Vegsoup-method}
\alias{map,Vegsoup-method}
\alias{MatrixFill,Vegsoup-method}
\alias{names,Vegsoup-method}
\alias{names<-,Vegsoup,character-method}
\alias{ncell,Vegsoup-method}
\alias{ncol,Vegsoup-method}
\alias{nrow,Vegsoup-method}
\alias{plot,Vegsoup,missing-method}
\alias{QuickMap,Vegsoup-method}
\alias{rbind,Vegsoup-method}
\alias{richness,Vegsoup-method}
\alias{rowMeans,Vegsoup-method}
\alias{rownames,Vegsoup,missing,missing-method}
\alias{rowSums,Vegsoup-method}
\alias{SampleVegsoup,Vegsoup-method}
\alias{seriation,Vegsoup-method}
\alias{Sites,Vegsoup-method}
\alias{Sites<-,Vegsoup,data.frame-method}
\alias{SpatialPointsVegsoup,Vegsoup-method}
\alias{SpatialPolygonsVegsoup,Vegsoup-method}
\alias{Species,Vegsoup-method}
\alias{Species<-,Vegsoup,SpeciesTaxonomy-method}
\alias{SpeciesList,Vegsoup-method}
\alias{Stride,Vegsoup-method}
\alias{summary,Vegsoup-method}
\alias{tail,Vegsoup-method}
\alias{Taxonomy,Vegsoup-method}
\alias{Taxonomy<-,Vegsoup,SpeciesTaxonomy-method}
\alias{vegdist,Vegsoup-method}
\alias{vegdist<-,Vegsoup,character-method}
\alias{write.verbatim,Vegsoup-method}

\title{ Class \code{"Vegsoup"} }
\description{
  This is the foundation class of all \code{Vegsoup*} objects.
}
\section{Objects from the Class}{

  Objects can be created by calls of the form \code{new("Vegsoup", ...)}. The
  creator function \code{\link{Vegsoup}} might be more more convinient and
  transparent.

}
\section{Slots}{
  \describe{
    \item{\code{species}:}{
      Object of class \code{"data.frame"},
      the species data in \emph{long} format.
    }
    \item{\code{sites}:}{
      Object of class \code{"data.frame"},
      the sites data in \emph{wide} format.
    }
    \item{\code{taxonomy}:}{
      Object of class \code{"data.frame"},
      the taxonomic reference list.
    }
    \item{\code{coverscale}:}{
      Object of class \code{"Coverscale"},
      the description of the cover scale. See
      \code{\linkS4class{Coverscale}} for details.
    }
    \item{\code{decostand}:}{
      Object of class \code{"decostand"},
      the method for \code{\link[vegsoup:decostand]{decostand}}. Takes effect
      if \code{\link{as.matrix}} or \code{\link{as.dist}} is called on object.
    }
    \item{\code{dist}:}{
      Object of class \code{"character"},
      giving a method for \code{\link{vegsoup:vegdist}}. Takes effect if
      \code{\link{as.dist}} is called on object.
    }
    \item{\code{layers}:}{
      Object of class \code{"character"},
      containing the layers the object is structered by.
    }
    \item{\code{group}:}{
      Object of class \code{"integer"},
      a vector of a-priori group assignments.
    }
    \item{\code{sp.points}:}{
      Object of class \code{"SpatialPointsDataFrame"}
      see \code{\link{SpatialPoints}} in package \pkg{sp}.
    }
    \item{\code{sp.polygons}:}{
      Object of class \code{"SpatialPolygonsDataFrame"}
      see \code{\link{SpatialPolygons}} in package \pkg{sp}.
    }
  }
}


\section{Methods}{
  \describe{
%% 1
  \bold{ 1) Retrieve species matrix.}
    In fact there are several ways of how to obtain the species matrix from an
    \code{Vegsoup*} object and all are based on the logic of object coercion.
    There are methods focused on a specific storage mode, as well, as a
    generic \code{as.matrix} coercion method. The following section outlines
    even more general index methods.
    \item{as.character}{
%     \code{signature(x = "Vegsoup")}:
      returns a matrix of mode "numeric" with species abundances in the data
      set's original scale.
    }
    \item{as.logical}{
%     \code{signature(x = "Vegsoup")}:
      returns a matrix of mode "numeric" with zeros coding for species
      absences and ones for presences.
    }
    \item{as.numeric}{
%     \code{signature(x = "Vegsoup")}:
      returns a matrix of mode "numeric" with species abundances coded by
      according to \code{coverscale(obj)}.
    }
    \item{as.matrix}{
%     \code{signature(x = "Vegsoup")}:
        retrieves a species matrices like those outlined above by setting the
        \code{typeof} argument specifing the mode of the returned matrix; can
        be one of \code{"character"}, \code{"numeric"} or \code{"logical"}.
        Use the additionla argument \code{mode} with all four methods to get
        matrix transpositions. See \code{\link{as.matrix}}.
    }

%% 2
  \bold{ 2) Dimensions of the object (species matrix).}
    \item{dim}{
%     \code{signature(x = "Vegsoup")}:
      returns matrix dimensions (see above) giving the number of plots and
      (pseudo-)species.
    }
    \item{ncol}{
%     \code{signature(x = "Vegsoup")}:
      the number of (pseudo-)species.
    }
    \item{nrow}{
%     \code{signature(x = "Vegsoup")}:
      the number of plots.
    }
    \item{ncell}{
%     \code{signature(x = "Vegsoup")}:
      the number of cells in the species matrix.
    }
    \item{MatrixFill}{
      \code{signature(obj = "Vegsoup")}:
      the percentage of presences in the species matrix.
    }
%% 3
  \bold{ 3) Retrieve indices to species matrix.}
    \item{indices}{
%     \code{signature(x = "Vegsoup")}:
      returns the locations (row and column pointers) and values of all
      nonzero entries in a species matrix (see above).
    }
    \item{as.vector}{
      \code{signature(x = "Vegsoup", mode = "missing")}:
      is a shortcut to \code{as.vector(as.matrix(x, typeof = mode), mode)}.
      Note, here the \code{'mode'} argument has the meaning of the base
      \code{R} function and specifies the storge mode od the returned vector.
     }
   Together, \code{indices} and \code{as.vector} can be used to create a
   species matrix. See \code{\link{as.matrix}}.
%% 4
  \bold{ 4) Retrieve dissimilarity/distance matrix.}
    \item{vegdist}{
%     \code{signature(x = "Vegsoup")}:
      returns the dissimilarity/distance index set for an object.
    }
    \item{vegdist<-}{
      \code{signature(x = "Vegsoup", value = "character")}:
      set a distance measure for an object.
    }
    \item{as.dist}{
      \code{signature(m = "Vegsoup")}:
      compute a distance matrix as specified by \code{vegdist(obj)}.
      See \code{\link{as.dist}}
    }
    \item{getDistconnected}{
      \code{signature(obj = "Vegsoup")}:
      tests if dissimilarities/distances are connected.
    }
%% 4
  \bold{ 3) Retrieve class slots.}
    Methods to access or replace the \code{'species'}, \code{'sites'}
    or \code{'taxonomy'} slots are:
    \item{Species}{
      \code{signature(obj = "Vegsoup")}:
      returns slot, on object of class
      \code{"data.frame"}, equaling an object of class
      \code{\linkS4class{Species}}.
    }
    \item{Species<-}{
      \code{signature(obj = "Vegsoup", value = "SpeciesTaxonomy")}:
      assign object of class \code{"SpeciesTaxonomy"} to an existing
      \code{Vegsoup*} object. Not implemented yet! See
      \code{\linkS4class{SpeciesTaxonomy}}.
    }
    \cr
    \item{Sites}{
      \code{signature(obj = "Vegsoup")}:
      returns slot, an object of class
      \code{"data.frame"}.
      %% equaling an object of class \code{\linkS4class{Sites}}.
    }
    \item{Sites<-}{
      \code{signature(obj = "Vegsoup", value = "data.frame")}:
      The value object for the replacement function needs to have rownames
      matching \code{rownames(obj)}.
    }
    \cr
    \item{Taxonomy}{
      \code{signature(obj = "Vegsoup")}:
      returns slot, an object of class
      \code{"data.frame"}, equaling an object of class
      \code{\linkS4class{Taxonomy}}.
    }
    \item{Taxonomy<-}{
      \code{signature(obj = "Vegsoup", value = "SpeciesTaxonomy")}:
      assign object of class \code{"SpeciesTaxonomy"} to an existing
      \code{Vegsoup*} object. Not implemented yet! See
      \code{\linkS4class{SpeciesTaxonomy}}
    }
%% 1
  \bold{ 1) Extract and replace.}
    \item{[}{
      \code{signature(x = "Vegsoup", i = "ANY", j = "ANY")}:
      provides all common indexing operations allowed for class
      \code{"matrix"}.
    }
    \item{[<-}{
      \code{signature(x = "Vegsoup", i = "ANY", j = "missing")}:
      allows assigments to the \code{'sites'} slot. Note, this does not
      affect the species matrix.
    }
    \item{$}{
%     \code{signature(x = "Vegsoup")}:
      access columns of sites data by name.
    }
    \item{$<-}{
%     \code{signature(x = "Vegsoup")}:
      assign columns of sites data by name.
    }
%% 1
  \bold{ 1) Reorder and sample.}
    \item{seriation}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{SampleVegsoup}{
%     \code{signature(x = "Vegsoup")}:
    }

%% 1
  \bold{ 1) Combine objects.}
    \item{rbind}{
      \code{signature(... = "Vegsoup")}:
    }
%% 1
  \bold{ 1) Names and species Abbreviations.}
    \item{rownames}{
%     \code{signature(x = "Vegsoup")}:
    }
    \item{colnames}{
%     \code{signature(x = "Vegsoup")}:
      returns the column names of the species matrix.
    }
    \item{dimnames}{
%     \code{signature(x = "Vegsoup")}:
    }
    \item{abbr.layer}{
      \code{signature(obj = "Vegsoup")}:
       Get abbreviations used to encode (pseudo-)species.
    }
    \item{DecomposeNames}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{Abbreviation}{
      \code{signature(obj = "Vegsoup")}:
      Get abbreviations used to encode species.
    }
    \item{names}{
%     \code{signature(x = "Vegsoup")}:
    }
    \item{names<-}{
      \code{signature(x = "Vegsoup", value = "character")}:
    }
    \item{AprioriGrouping}{
      \code{signature(obj = "Vegsoup")}:
    }
%% 1
  \bold{ 1) Layer structure.}

    \item{Layer}{
      \code{signature(obj = "Vegsoup")}:
      returns the layer for each species observation.
    }
    \item{Layers}{
      \code{signature(obj = "Vegsoup")}:
      modifies an object's layer structure by suppling a layer combination
      rule as a \code{"character"} vector \code{'collapse'}.
    }
    \item{Layers<-}{
      \code{signature(obj = "Vegsoup")}:
      modifies the order of layers. This has effects on \code{as.matrix} and
      similar.
    }
%% 1
  \bold{ 1) Cover scale.}
    \item{BraunBlanquetReduce}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{coverscale}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{coverscale<-}{
      \code{signature(obj = "Vegsoup", value = "character")}:
    }
    \item{coverscale<-}{
      \code{signature(obj = "Vegsoup", value = "Coverscale")}:
    }

%% 1
  \bold{ 1) Standardization methods.}
    \item{decostand}{
       \code{signature(obj = "Vegsoup")}:
       returns the standardization method active for an object.
    }
    \item{decostand<-}{
      \code{signature(obj = "Vegsoup", value = "character")}:
       sets the standardization method active for an object. Note,
       values of slot \code{decostand} have effect in \code{as.matrix} and
       similar matrix methods.
      \code{signature(obj = "Vegsoup", value = "NULL")}:
       reset/remove the standardization for an object by setting the value to
       \code{NULL}.
    }

%% 1
  \bold{ 1) Coerce methods.}
    \item{coerce}{
      \code{signature(from = "Vegsoup", to = "dist")}:
      same as \code{as.dist(obj)} (see above).
      \code{signature(from = "Vegsoup", to = "list")}:
      returns objects all of class \code{"data.frame"} from slots
      \code{'species'}, \code{'sites'} and \code{'taxonomy'}.
      \code{signature(from = "Vegsoup", to = "matrix")}:
      same as \code{as.matrix(obj)} (see above).
      \code{signature(from = "Vegsoup", to = "dsparseMatrix")}:
      returns objects all of class \code{"dsparseMatrix"}. See
      \code{\link{as.matrix}}. %% improve doc
      \code{signature(from = "Vegsoup", to = "sparseMatrix")}:
      returns objects all of class \code{"sparseMatrix"}. See
      \code{\link{as.matrix}}. %% improve doc
    }
%% 1
  \bold{ 1) Plotting methods.}
    \item{plot}{
      \code{signature(x = "Vegsoup", y = "missing")}:
    }
    \item{coldiss}{
      \code{signature(obj = "Vegsoup")}:
      plots of the dissimilarity matrix.
    }
    \item{map}{
      \code{signature(obj = "Vegsoup")}:
    }

%% 1
  \bold{ 1) Row and colum aggregation.}
    \item{colMeans}{
%     \code{signature(x = "Vegsoup")}:
      returns \code{colMeans(as.matrix(obj))}.
    }
    \item{colSums}{
%     \code{signature(x = "Vegsoup")}:
      returns \code{colSums(as.matrix(obj))}
    }
    \item{rowSums}{
%     \code{signature(x = "Vegsoup")}:
    }
    \item{richness}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{rowMeans}{
%     \code{signature(x = "Vegsoup")}:
    }
%% 1
  \bold{ 1) Spatial methods.}
    \item{coordinates}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{elevation}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{KML}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{SpatialPointsVegsoup}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{SpatialPolygonsVegsoup}{
      \code{signature(obj = "Vegsoup")}:
    }
%% 1
  \bold{ 1) Summary methods.}
    \item{summary}{
      \code{signature(object = "Vegsoup")}:
    }
    \item{head}{
%     \code{signature(x = "Vegsoup")}:
    }
    \item{tail}{
%     \code{signature(x = "Vegsoup")}:
    }
    \item{SpeciesList}{
      \code{signature(obj = "Vegsoup")}:
    }
%% 1
  \bold{ 1) Special output methods.}
    \item{QuickMap}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{write.verbatim}{
      \code{signature(obj = "Vegsoup")}:
    }
%% 1
  \bold{ 1) Miscellaneous.}
    \item{Indpower}{
      \code{signature(obj = "Vegsoup")}:
    }
    \item{Indspc}{
      \code{signature(obj = "Vegsoup")}:
    }

  }
}
\author{ Roland Kaiser }
\note{
  Classes \code{"VegsoupPartition"} and \code{"VegsoupPartition"} all extend
  class \code{"Vegsoup"}.
}

\seealso{
\code{\linkS4class{Species}},
\code{\linkS4class{Sites}},
\code{\linkS4class{Taxonomy}},
\code{\linkS4class{Vegsoup}},
\code{\linkS4class{instantsoup}}
}
\examples{
showClass("Vegsoup")
}
\keyword{ classes }
\keyword{ methods }
