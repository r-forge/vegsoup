\name{Phi}
\docType{methods} 
\alias{Phi}
\alias{Phi-methods}
\alias{Phi,VegsoupDataPartition-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the phi Statistic for Objects if Class VegsoupDataPartition
}
\description{
Calculates the \emph{phi} statistic based on the partitioning of the object.
}
\usage{
Phi(obj)
}
\arguments{
  \item{obj}{
An object of class \code{"VegsoupDataPartition"}.
}
}
\details{
\emph{phi} is a statistic of agreement between two vectors. See \code{\link[optpart:phi]{phi}} in package \pkg{optpart} for details.
}
\value{
A \code{matrix} of \emph{phi} values with species as rows and partitions as columns}
\references{
Tichy, L. and M. Chytry. 2006. Statistical determination of diagnostic species for site groups of unequal size. Journal of Vegetation Science 17:809-818.
}
\author{
Roland Kaiser
}
\note{
This is a slightly faster compared to \code{\link{Fidelity}}, but does not return an \code{Vegsoup*} object.
}
\seealso{
\code{\link[optpart:phi]{phi}}
}
\keyword{ methods }

