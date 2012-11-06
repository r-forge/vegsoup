\name{SampleVegsoup}
\alias{SampleVegsoup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Samples and Permutations of Objects extending VegsoupData
}
\description{
sample takes a sample of the specified size from the elements of x using either with or without replacement.
}
\usage{
SampleVegsoup(x, size, replace = FALSE, prob = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Either a vector of one or more elements from which to choose, or a positive integer.
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{replace}{
a non-negative integer giving the number of items to choose.
}
  \item{prob}{
A vector of probability weights for obtaining the elements of the vector being sampled.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
See \code{\link{sample}}.
}
\value{
An object of same class as the input class.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\keyword{methods}

