\encoding{UTF-8}

\name{seriation}
\docType{methods}

\alias{seriation}
\alias{seriation,Vegsoup-method}
\alias{seriation,VegsoupPartition-method}

\title{ Reorder as to Reflect a Vegetation Table }

\description{
The method rearranges the species matrix according to ordination coordinates, cluster analysis or by species frequencies to achieve a \sQuote{structured} table following phytosociological principles. A modified object is returned as to reflect the order of plots and species as returned by the selected method.
}

\usage{
\S4method{seriation}{Vegsoup}(obj, method, mode, ...)

\S4method{seriation}{VegsoupPartition}(obj, method, mode, ...)

}
\arguments{
  \item{ obj }{
	A \code{Vegsoup*} object.
  }
  \item{ method }{
	Method applied to obtain new order of data set. If missing defaults to
	\code{"dca"}. See \sQuote{Details}.
  }
  \item{ mode }{
	\code{Q} or \code{R} mode analysis passed to \code{\link{as.matrix}}.
  }
  \item{ \dots }{
	Additional arguments passed to \code{\link[vegan:decorana]{decorana}}.
	See \sQuote{Note}.
  }
}
\details{
For objects of class \verb{Vegsoup} the default method \code{"dca"} uses function \code{\link{decorana}} in \pkg{vegan} with default settings. Method \code{"hclust"} uses \code{\link{hclust}} with argument \code{method = "ward"}, \code{"ward"} calls \code{\link{agnes}} with argument \code{method = "ward"} and \code{"flexible"} invokes \code{agnes} with arguments \code{method = "flexible"} and \code{par.method = c(0.625, 0.625, 1 - 2 * 0.625, 0)}. Finally, method \code{"packed"} simply performs ordering by row and col sums.

The method defined for class \code{"VegsoupPartition"} equals separate calls to subsets defined by \code{partitioning(obj)}, same as a call to objects of class \verb{Vegsoup}, followed by final combination of all subsets into a single object. Note that this orders plots within each group based on the defined method, but the order of species is determined by order they appear in single subsets. So the order of species might not be satisfactory.

Ordering by explicit indexing is available by using the method \code{\link[vegsoup]{Extract}} method for \code{Vegsoup*} objects.
}
\value{
An object of class depending of the input class.
}
\references{
Missing!
}
\author{
Roland Kaiser
}
\note{
Currently there are only very limited ways to pass down arguments to functions and. Only presence/absence data is used in. Arguments to method \code{"dca"} (\code{\link{decorana}}, \code{"iweigh"}, \code{ira}, etc. ) can be specified using the "\dots" argument.
}
\seealso{
\code{\link{vegemite}} in package \pkg{vegan} for the ancestor of all vegetation table functions,
\code{\link[labdsv:ordtaxa]{ordtaxa}} for an interactive implementation in package \pkg{labdsv},
and \code{\link{VegsoupPartition}} for creating a partitions.
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

rownames(seriation(x))
rownames(seriation(x, "packed"))

# use default print method of package Matrix
require(Matrix)
# zero are printed as dots and decimals are truncated to 0
t(as(seriation(x, "dca"), "sparseMatrix"))
}
\keyword{ methods }

