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
\alias{Abbreviation<-,Vegsoup,character-method}
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
  This is the basic class of all \code{Vegsoup*} objects.
}
\section{Objects from the Class}{

  Objects can be created by calls of the form \code{new("Vegsoup", ...)}, but
  the creator functions \code{\link{Vegsoup}} are much more convinient, as
  there are many restrictions about how the data is structurted.

}
\section{Slots}{
  \describe{
    \item{\code{species}:}{
      Object of class \code{"data.frame"} ~~ 
    }
    \item{\code{sites}:}{
       Object of class \code{"data.frame"} ~~ 
    }
    \item{\code{taxonomy}:}{
      Object of class \code{"data.frame"} ~~
    }
    \item{\code{coverscale}:}{
      Object of class \code{"Coverscale"} ~~
    }
    \item{\code{decostand}:}{
      Object of class \code{"decostand"} ~~
    }
    \item{\code{dist}:}{
      Object of class \code{"character"} ~~
    }
    \item{\code{layers}:}{
      Object of class \code{"character"} ~~
    }
    \item{\code{group}:}{
      Object of class \code{"integer"} ~~
    }
    \item{\code{sp.points}:}{
      Object of class \code{"SpatialPointsDataFrame"} ~~
    }
    \item{\code{sp.polygons}:}{
      Object of class \code{"SpatialPolygonsDataFrame"} ~~
    }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Vegsoup", i = "ANY", j = "ANY", drop = "missing")}: ... }
    \item{[<-}{\code{signature(x = "Vegsoup", i = "ANY", j = "missing")}: ... }
    \item{$}{\code{signature(x = "Vegsoup")}: ... }
    \item{$<-}{\code{signature(x = "Vegsoup")}: ... }
    \item{abbr.layer}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Abbreviation}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Abbreviation<-}{\code{signature(obj = "Vegsoup", value = "character")}: ... }
    \item{AprioriGrouping}{\code{signature(obj = "Vegsoup")}: ... }
    \item{as.character}{\code{signature(x = "Vegsoup")}: ... }
    \item{as.dist}{\code{signature(m = "Vegsoup")}: ... }
    \item{as.logical}{\code{signature(x = "Vegsoup")}: ... }
    \item{as.matrix}{\code{signature(x = "Vegsoup")}: ... }
    \item{as.numeric}{\code{signature(x = "Vegsoup")}: ... }
    \item{as.vector}{\code{signature(x = "Vegsoup", mode = "missing")}: ... }
    \item{BraunBlanquetReduce}{\code{signature(obj = "Vegsoup")}: ... }
    \item{coerce}{\code{signature(from = "Vegsoup", to = "dist")}: ... }
    \item{coerce}{\code{signature(from = "Vegsoup", to = "dsparseMatrix")}: ... }
    \item{coerce}{\code{signature(from = "Vegsoup", to = "list")}: ... }
    \item{coerce}{\code{signature(from = "Vegsoup", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "Vegsoup", to = "sparseMatrix")}: ... }
    \item{coldiss}{\code{signature(obj = "Vegsoup")}: ... }
    \item{colMeans}{\code{signature(x = "Vegsoup")}: ... }
    \item{colnames}{\code{signature(x = "Vegsoup")}: ... }
    \item{colSums}{\code{signature(x = "Vegsoup")}: ... }
    \item{coordinates}{\code{signature(obj = "Vegsoup")}: ... }
    \item{coverscale}{\code{signature(obj = "Vegsoup")}: ... }
    \item{coverscale<-}{\code{signature(obj = "Vegsoup", value = "character")}: ... }
    \item{coverscale<-}{\code{signature(obj = "Vegsoup", value = "Coverscale")}: ... }
    \item{DecomposeNames}{\code{signature(obj = "Vegsoup")}: ... }
    \item{decostand}{\code{signature(obj = "Vegsoup")}: ... }
    \item{decostand<-}{\code{signature(obj = "Vegsoup", value = "character")}: ... }
    \item{decostand<-}{\code{signature(obj = "Vegsoup", value = "NULL")}: ... }
    \item{dim}{\code{signature(x = "Vegsoup")}: ... }
    \item{dimnames}{\code{signature(x = "Vegsoup")}: ... }
    \item{elevation}{\code{signature(obj = "Vegsoup")}: ... }
    \item{getDistconnected}{\code{signature(obj = "Vegsoup")}: ... }
    \item{head}{\code{signature(x = "Vegsoup")}: ... }
    \item{indices}{\code{signature(x = "Vegsoup")}: ... }
    \item{Indpower}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Indspc}{\code{signature(obj = "Vegsoup")}: ... }
    \item{KML}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Layer}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Layers}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Layers<-}{\code{signature(obj = "Vegsoup")}: ... }
    \item{map}{\code{signature(obj = "Vegsoup")}: ... }
    \item{MatrixFill}{\code{signature(obj = "Vegsoup")}: ... }
    \item{names}{\code{signature(x = "Vegsoup")}: ... }
    \item{names<-}{\code{signature(x = "Vegsoup", value = "character")}: ... }
    \item{ncell}{\code{signature(x = "Vegsoup")}: ... }
    \item{ncol}{\code{signature(x = "Vegsoup")}: ... }
    \item{nrow}{\code{signature(x = "Vegsoup")}: ... }
    \item{plot}{\code{signature(x = "Vegsoup", y = "missing")}: ... }
    \item{QuickMap}{\code{signature(obj = "Vegsoup")}: ... }
    \item{rbind}{\code{signature(... = "Vegsoup")}: ... }
    \item{richness}{\code{signature(obj = "Vegsoup")}: ... }
    \item{rowMeans}{\code{signature(x = "Vegsoup")}: ... }
    \item{rownames}{\code{signature(x = "Vegsoup", do.NULL = "missing", prefix = "missing")}: ... }
    \item{rowSums}{\code{signature(x = "Vegsoup")}: ... }
    \item{SampleVegsoup}{\code{signature(x = "Vegsoup")}: ... }
    \item{seriation}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Sites}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Sites<-}{\code{signature(obj = "Vegsoup", value = "data.frame")}: ... }
    \item{SpatialPointsVegsoup}{\code{signature(obj = "Vegsoup")}: ... }
    \item{SpatialPolygonsVegsoup}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Species}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Species<-}{\code{signature(obj = "Vegsoup", value = "SpeciesTaxonomy")}: ... }
    \item{SpeciesList}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Stride}{\code{signature(obj = "Vegsoup")}: ... }
    \item{summary}{\code{signature(object = "Vegsoup")}: ... }
    \item{tail}{\code{signature(x = "Vegsoup")}: ... }
    \item{Taxonomy}{\code{signature(obj = "Vegsoup")}: ... }
    \item{Taxonomy<-}{\code{signature(obj = "Vegsoup", value = "SpeciesTaxonomy")}: ... }
    \item{vegdist}{\code{signature(x = "Vegsoup")}: ... }
    \item{vegdist<-}{\code{signature(x = "Vegsoup", value = "character")}: ... }
    \item{write.verbatim}{\code{signature(obj = "Vegsoup")}: ... }
	 }
}
\author{ Roland Kaiser }
\note{
%%  ~~further notes~~
}

\seealso{
\code{\linkS4class{Species}},
\code{\linkS4class{Sites}},
\code{\linkS4class{Taxonomy}}
}
\examples{
showClass("Vegsoup")
}
\keyword{ classes }
\keyword{ methods }
