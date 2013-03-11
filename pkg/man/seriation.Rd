\name{seriation}
\docType{methods}
\alias{seriation}
\alias{seriation-methods}

\title{ Modify an Objects Internal Order as to Reflect a Vegetation Table }
\description{
The method rearranges the species matrix according to ordination coordinates, cluster analysis or by species frequencies to achieve a \sQuote{structured} table following phytosociological principles. A modified object is returned as to reflect the order of plots and species as returned by the selected method.
}
\usage{

## S4 method for signature 'Vegsoup'
seriation(object, method, ...)

## S4 method for signature 'VegsoupPartition'
seriation(object, method, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ obj }{
A \code{Vegsoup*} object.
}
  \item{ methods }{
Methods applied to obtain new order of data set. See \sQuote{Details}. If missing defaults to \code{"dca"}. See \sQuote{Details}.
}
  \item{ \dots }{
Arguments passed to \code{\link[vegan:decorana]{decorana}}. See \sQuote{Note}.
}
}
\details{
For objects of class \code{Vegsoup} the default method \code{"dca"} uses function \code{\link{decorana}} in \pkg{vegan} with default settings. Method \code{\dQuote{hclust}} uses \code{\link{hclust}} with argument \code{method = "ward"}, \code{"ward"} calls \code{\link{agnes}} with argument \code{method = "ward"} and \code{"flexible"} invokes \code{agnes} with arguments \code{method = "flexible"} and \code{par.method = c(0.625, 0.625, 1 - 2 * 0.625, 0)}. Finally, method \code{"packed"} simply performs ordering by row and col sums.

The method defined for class \code{"VegsoupPartition"} equals separate calls to subsets defined by \code{Partitioning(obj)}, same as a call to objects of class \code{Vegsoup}, followed by final combination of all subsets into a single object. Note that this orders plots within each group based on the defined method, but the order of species is determined by order they appear in single subsets. So the order of species might not be satisfactory.

Ordering by explicit indexing is available by using the extract method \code{\link[vegsoup:extract]{'['}} for \code{Vegsoup*} objects.
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
\code{\link{ordtaxa}} for an interactive implementation in package \pkg{labdsv},
and \code{\link{VegsoupPartition}} for a feature-rich interface.
}

\examples{
data(testdata)
dta <- Vegsoup(spc, sts, txa,
       coverscale = "braun.blanquet")
seriation(dta)
seriation(dta, "packed")	
}
\keyword{ methods }

