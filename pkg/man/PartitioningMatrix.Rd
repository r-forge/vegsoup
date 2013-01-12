\name{PartitioningMatrix}
\docType{methods} 
\alias{PartitioningMatrix}
\alias{PartitioningMatrix-methods}
\alias{PartitioningMatrix,VegsoupDataPartition-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tabulate Partitioning Vector of an Object of Class VegsoupDataPartition to a Matrix
}
\description{
Create a matrix of memberships of plots to partitions.
}
\usage{
PartitioningMatrix(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
An object of class \code{"VegsoupDataPartition"}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a \code{matrix} of zeros and ones where rows are plots and columns are partitons. One indicate to which partition a plot belongs.
}
\author{
Roland Kaiser
}

\seealso{
\code{\link{Partitioning}}, ~~~
}
\keyword{ methods }

