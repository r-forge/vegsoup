\encoding{UTF-8}

\name{VegsoupPartition-class}
\Rdversion{1.1}
\docType{class}

\alias{VegsoupPartition-class}

\title{ VegsoupPartition Class }

\description{
  Class \code{"VegsoupPartition"} is a class directly extending class
  \code{\linkS4class{Vegsoup}} and represents a partitioning of an object into
  clusters.
}

\section{ Objects from the Class }{
  Objects can be created by calls of the form \code{new("VegsoupPartition",
  ...)} where \dots expand to slots \code{'part'}, \code{'method'} and
  \code{'k'}. It is strongly advisable to use the class creator function
  \code{\link{VegsoupPartition}}.
}

\section{ Slots }{
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
\section{ Extends }{
  Class \code{"VegsoupPartition"} extends \code{"\linkS4class{Vegsoup}"}
  directly and all methods defined for the latter class apply.
}
\section{ Methods }{
  Methods defined for class \code{"VegsoupPartition"} include the following:
  \describe{
%% 1
  \bold{ 1) Getter methods }
   \item{partitioning}{
	 \code{signature(obj = "VegsoupPartition")}:
	 Return partitioning (clustering) vector.
	 See \code{\link{partitioning}}.
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
	\item{partition}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Retrieve a single partition by it's value.
	  See \code{\link{partition}}.
	}
	\item{seriation}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Modify an objects internal order as to reflect a vegetation table.
	  See \code{\link{seriation}}.
	}
%% 3
  \bold{ 3) Replacement }
	\item{partitioning<-}{
	  \code{signature(obj = "VegsoupPartition", value = "numeric")}:
	  Experimental.
	  See \code{\link{partitioning}}. %%!
	}
%% 4
  \bold{ 4) Object summary and print methods }
	\item{summary}{
	  \code{signature(object = "VegsoupPartition")}:
	  Summary method for \code{"VegsoupPartition"} objects.
%	 See \code{\link{}}.
	}
	\item{head}{
	  \code{signature(x = "VegsoupPartition")}:
	  Print method.
	  See \code{\link{head}}.
	}
%% 5
  \bold{ 5) Cluster optimization methods }
	\item{optindval}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Optimize partition by maximizing Dufrene and Legendre's Indicator
	  value.
	  See \code{\link{optindval}}.   
	}
	\item{optsil}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Optimize partition by maximizing mean silhouette width of partitions.
	  See \code{\link{optsil}}.
	}
%% 6
  \bold{ 6) Table Summaries }
	\item{contingency}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Contingency table.
	  See \code{\link{contingency}}.
	}
	\item{constancy}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Constancy table.
	  See \code{\link{constancy}}
	}
	\item{importance}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Importance table.
	  See \code{\link[labdsv]{importance}}.
	}
%% 7
  \bold{ 7) Descriptive statistics and and additional summaries }
	\item{quantile}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Sample quantiles for each species in each partition (cluster).
	  See \code{\link{quantile}}.
	}
	\item{FisherTest}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Partitioning (classification) validity based Fisher Text.
	  See \code{\link{FisherTest}}.
	}
	\item{Tabdev}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Partition (classification) validity assessment by table deviance.
%	 See \code{\link{}}.	
	}
	\item{Partana}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Calculate mean object-to-partition(cluster) similarity.
	  See \code{\link{partana}}.
	}
	\item{richness}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Retrieve dataset, sample or species richness grouped by
	  partitions (clusters).
	  See \code{\link{richness}}.
	}
	\item{spread}{
	  \code{signature(object = "VegsoupPartition")}:
	  List occurrences of species in partitions (clusters).
%	 See \code{\link{}}.
	}
	\item{shared}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Computes a diagonal matrix showing percentages of shared species between
	  partitions (clusters).
	  See \code{\link{shared}}.
	}
	\item{typal}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Identifies samples (plots) that typify clusters in a partition based on
	  dissimilarity.
	  See \code{\link{typal}}.
	}
	\item{partitioningMatrix}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Create a matrix of memberships of plots to partitions.
	  See \code{\link{partitioningMatrix}}.
	}
	\item{partitioningCombinations}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Tabulate all possible combination of partitions.
	  See \code{\link{partitioningCombinations}}.
	}
%% 8
  \bold{ 8) Cluster validation }
	\item{disdiam}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Maximum within-cluster dissimilarity of all partitions.
	  See \code{\link{disdiam}}.
	}
	\item{silhouette}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Compute silhouette information.
	  See \code{\link{silhouette}}.
	}
%% 9
  \bold{ 9) Indicator species (fidelity) methods}
	\item{fidelity}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Calculate fidelity values and promote to class
	  \code{"VegsoupPartitionFidelity"}.
	  See \code{\link{fidelity}} and
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
	\item{isamic}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Indicator species analysis to minimize intermediate constancy
	  See \code{\link{isamic}}.
	}
	\item{murdoch}{
	  \code{signature(obj = "VegsoupPartition")}:
	  Indicator Species Analysis by Murdoch Preference Function.
	  See \code{\link{murdoch}}.
	}
%% 10
  \bold{ 10) Plot methods }
	\item{plot}{
	  \code{signature(x = "VegsoupPartition", y = "missing")}:
	  Ordination plot highlighting partitions.
%	 See \code{\link{}}.
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

