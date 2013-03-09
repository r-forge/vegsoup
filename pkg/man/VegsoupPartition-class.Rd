\encoding{UTF-8}
\name{VegsoupPartition-class}
\Rdversion{1.1}
\docType{class}
\alias{VegsoupPartition-class}
\alias{[,VegsoupPartition,ANY,ANY,missing-method}
\alias{Accuracy,VegsoupPartition,VegsoupPartition-method}
\alias{Association,VegsoupPartition,VegsoupPartition-method}
\alias{Confus,VegsoupPartition,VegsoupPartition-method}
\alias{Constancy,VegsoupPartition-method}
\alias{Contingency,VegsoupPartition-method}
\alias{Disdiam,VegsoupPartition-method}
\alias{ellipsoidhull,VegsoupPartition-method}
\alias{Fidelity,VegsoupPartition-method}
\alias{FisherTest,VegsoupPartition-method}
\alias{Fivenum,VegsoupPartition-method}
\alias{getK,VegsoupPartition-method}
\alias{head,VegsoupPartition-method}
\alias{Importance,VegsoupPartition-method}
\alias{Indval,VegsoupPartition-method}
\alias{Isamic,VegsoupPartition-method}
\alias{KML,VegsoupPartition-method}
\alias{Latex,VegsoupPartition-method}
\alias{Murdoch,VegsoupPartition-method}
\alias{Optindval,VegsoupPartition-method}
\alias{Optsil,VegsoupPartition-method}
\alias{Partana,VegsoupPartition-method}
\alias{Partition,VegsoupPartition-method}
\alias{Partitioning,VegsoupPartition-method}
\alias{Partitioning<-,VegsoupPartition,numeric-method}
\alias{PartitioningCombinations,VegsoupPartition-method}
\alias{PartitioningMatrix,VegsoupPartition-method}
\alias{Phi,VegsoupPartition-method}
\alias{plot,VegsoupPartition,missing-method}
\alias{rectangles,VegsoupPartition-method}
\alias{richness,VegsoupPartition-method}
\alias{seriation,VegsoupPartition-method}
\alias{Shared,VegsoupPartition-method}
\alias{SigFidelity,VegsoupPartition-method}
\alias{Silhouette,VegsoupPartition-method}
\alias{Spread,VegsoupPartition-method}
\alias{summary,VegsoupPartition-method}
\alias{Tabdev,VegsoupPartition-method}
\alias{Typal,VegsoupPartition-method}
\alias{write.verbatim,VegsoupPartition-method}

\title{ Class \code{"VegsoupPartition"} }
\description{
  Class \code{"VegsoupPartition"} is a class directly extending class
  \code{\linkS4class{Vegsoup}} and is defined as having a partition
  (clustering) structure.
}
\section{Objects from the Class}{

  Objects can be created by calls of the form \code{new("VegsoupPartition",
  ...)} where \dots expand to slots \code{'part'}, \code{'method'} and
  \code{'k'}. It is strongly advisable to use the class creator function
  \code{\link{VegsoupPartition}}.

}
\section{Slots}{
  \describe{
    \item{\code{part}:}{
      Object of class \code{"numeric"}
      giving the partitioning (clustering) vector.
    }
    \item{\code{method}:}{
      Object of class \code{"character"}
      giving the method thta was used to create the object.
    }
    \item{\code{k}:}{
      Object of class \code{"numeric"}
      giving the number of partitions.
    }
  }
}
\section{Extends}{
  Class \code{"VegsoupPartition"} extends \code{"\linkS4class{Vegsoup}"}
  directly and all methods defined for the latter class apply.
}
\section{Methods}{
  Methods defined for class \code{"VegsoupPartition"} include the following:
  \describe{
%% 1
  \bold{ 1) Getter methods }
   \item{Partitioning}{
     \code{signature(obj = "VegsoupPartition")}:
     Return partitioning (clustering) vector.
     See \code{\link{Partitioning}}.
    }
    \item{getK}{
      \code{signature(obj = "VegsoupPartition")}:
      Return the number of partitions (clusters).
      See \code{\link{getK}}.
    }
%% 2
  \bold{ 2) Indexing }
    \item{[}{
      \code{signature(x = "VegsoupPartition", i = "ANY", j = "ANY")}:
      Traditional row and column indexing. Note, that subsetting may also
      affect class slots \code{'part'}, \code{'method'} and
      \code{'k'} if necessary.
    }
    \item{Partition}{
      \code{signature(obj = "VegsoupPartition")}:
      Retrieve a single partition by it's value.
      See \code{\link{Partition}}.
    }
    \item{seriation}{
      \code{signature(obj = "VegsoupPartition")}:
      Modify an objects internal order as to reflect a vegetation table.
      See \code{\link{seriation}}.
    }
%% 3
  \bold{ 3) Replacement }
    \item{Partitioning<-}{
      \code{signature(obj = "VegsoupPartition", value = "numeric")}:
      Experimental.
      See \code{\link{Partitioning}}. %%!
    }
%% 4
  \bold{ 4) Object summary and print methods }
    \item{summary}{
      \code{signature(object = "VegsoupPartition")}:
      Summary method for \code{"VegsoupPartition"} objects.
      See \code{\link{}}.
    }
    \item{head}{
      \code{signature(x = "VegsoupPartition")}:
      Print method.
      See \code{\link{}head}.
    }
%% 5
  \bold{ 5) Cluster optimization methods }
    \item{Optindval}{
      \code{signature(obj = "VegsoupPartition")}:
      Optimize partition by maximizing Dufrene and Legendre's Indicator
      value.
      See \code{\link{Optindval}}.   
    }
    \item{Optsil}{
      \code{signature(obj = "VegsoupPartition")}:
      Optimize partition by maximizing mean silhouette width of partitions.
      See \code{\link{Optsil}}.
    }
%% 6
  \bold{ 6) Table Summaries }
    \item{Contingency}{
      \code{signature(obj = "VegsoupPartition")}:
      Contingency table.
      See \code{\link{Contingency}}.
    }
    \item{Constancy}{
      \code{signature(obj = "VegsoupPartition")}:
      Constancy table.
      See \code{\link{Constancy}}
    }
    \item{Importance}{
      \code{signature(obj = "VegsoupPartition")}:
      Importance table.
      See \code{\link{Importance}}.
    }
%% 7
  \bold{ 7) Descriptive statistics and and additional summaries }
    \item{Fivenum}{
      \code{signature(obj = "VegsoupPartition")}:
      Tukeys Five-Number Summaries for each species in each partition
      (cluster).
      See \code{\link{Fivenum}}.
    }
    \item{FisherTest}{
      \code{signature(obj = "VegsoupPartition")}:
      Partitioning (classification) validity based Fisher Text.
      See \code{\link{FisherTest}}.
    }
    \item{Tabdev}{
      \code{signature(obj = "VegsoupPartition")}:
      Partitioning (classification) validity assessment by table deviance.
      See \code{\link{}}.    
    }
    \item{Partana}{
      \code{signature(obj = "VegsoupPartition")}:
      Calculate mean object-to-partition(cluster) similarity.
      See \code{\link{Partana}}.
    }
    \item{richness}{
      \code{signature(obj = "VegsoupPartition")}:
      Retrieve dataset, sample or species richness grouped by
      partitions (clusters).
      See \code{\link{richness}}.
    }
    \item{Spread}{
      \code{signature(object = "VegsoupPartition")}:
      List occurrences of species in partitions (clusters).
      See \code{\link{}}.
    }
    \item{Shared}{
      \code{signature(obj = "VegsoupPartition")}:
      Computes a diagonal matrix showing percentages of shared species between
      partitions (clusters).
      See \code{\link{Shared}}.
    }
    \item{Typal}{
      \code{signature(obj = "VegsoupPartition")}:
      Identifies samples (plots) that typify clusters in a partition based on
      dissimilarity.
      See \code{\link{Typal}}.
    }
    \item{PartitioningMatrix}{
      \code{signature(obj = "VegsoupPartition")}:
      Create a matrix of memberships of plots to partitions.
      See \code{\link{PartitioningMatrix}}.
    }
    \item{PartitioningCombinations}{
      \code{signature(obj = "VegsoupPartition")}:
      Tabulate all possible combination of partitions.
      See \code{\link{PartitioningCombinations}}.
    }
%% 8
  \bold{ 8) Cluster validation }
    \item{Disdiam}{
      \code{signature(obj = "VegsoupPartition")}:
      Maximum within-cluster dissimilarity) of all partitions.
      See \code{\link{Disdiam}}.
    }
    \item{Silhouette}{
      \code{signature(obj = "VegsoupPartition")}:
      Compute silhouette information according the objects inherent
      partitioning (clustering) vector.
      See \code{\link{Silhouette}}.
    }
%% 9
  \bold{ 9) Indicator species (fidelity) methods}
    \item{Fidelity}{
      \code{signature(obj = "VegsoupPartition")}:
      Calculate fidelity values and promote to class
      \code{"VegsoupPartitionFidelity"}.
      See \code{\link{Fidelity}} and
      \code{\linkS4class{VegsoupPartitionFidelity}}.
    }
    \item{SigFidelity}{
      \code{signature(obj = "VegsoupPartition")}:
      Experimental.
      See \code{\link{SigFidelity}}.
    }
    \item{Phi}{
      \code{signature(obj = "VegsoupPartition")}:
      Calculate Phi statistic based on partitioning vector.
      See \code{\link{Phi}}.
    }
    \item{Indval}{
      \code{signature(obj = "VegsoupPartition")}:
      Dufrene-Legendre Indicator Species Analysis
      See \code{\link{Indval}}.
    }
    \item{Isamic}{
      \code{signature(obj = "VegsoupPartition")}:
      Indicator species analysis to minimize intermediate constancy
      See \code{\link{Isamic}}.
    }
    \item{Murdoch}{
      \code{signature(obj = "VegsoupPartition")}:
      Indicator Species Analysis by Murdoch Preference Function.
      See \code{\link{Murdoch}}.
    }
%% 10
  \bold{ 10) Plot methods }
    \item{plot}{
      \code{signature(x = "VegsoupPartition", y = "missing")}:
      Ordination plot highlighting partitions.
      See \code{\link{}}.
    }
    \item{rectangles}{
      \code{signature(obj = "VegsoupPartition")}:
      Create a plot of matrix dimensions of each partition (cluster).
      See \code{\link{rectangles}}.
    }
    \item{ellipsoidhull}{
      \code{signature(x = "VegsoupPartition")}:
      Computes the \emph{ellipsoid hull} such that all given plot locations
      lie just inside or on the boundary of the ellipsoid and adds it to a
      map.
      See \code{\link{ellipsoidhull}}.
    }
%% 11
  \bold{ 11) Typesetting and text output methods }
    \item{Latex}{
      \code{signature(obj = "VegsoupPartition")}:
      Save a LaTex file-
      See \code{\link{Latex}}.
    }
    \item{write.verbatim}{
      \code{signature(obj = "VegsoupPartition")}:
      Output fixed column width table.
      See \code{\link{write.verbatim}}.
    }
%% 12
  \bold{ 12) Spatial output methods }
    \item{KML}{
      \code{signature(obj = "VegsoupPartition")}:
      Save KML file for visualization in Goolge Earth.
      See \code{\link{KML}}.
    }
%% 13
  \bold{ 1) Comparisons of two objects.}
    \item{Accuracy}{
      \code{signature(obj1 = "VegsoupPartition", obj2 = "VegsoupPartition")}:
      Accuracy Statistics of two objects under comparison.
      See \code{\link{Accuracy}}.
    }
    \item{Association}{
      \code{signature(obj1 = "VegsoupPartition", obj2 = "VegsoupPartition")}:
      Measures of association of two objects.
      See \code{\link{Association}}.
    }
    \item{Confus}{
      \code{signature(obj1 = "VegsoupPartition", obj2 = "VegsoupPartition")}:
      Confusion matrix of two objects.
      See \code{\link{Confus}}.
    }
  }
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{Vegsoup}}
}
\examples{
showClass("VegsoupPartition")
}
\keyword{classes}
