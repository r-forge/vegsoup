\name{head}
\alias{head}
\alias{head-methods}
\alias{head,VegsoupData-method}
\alias{head,VegsoupDataPartition-method}
\alias{tail}
\alias{tail-methods}
\alias{tail,VegsoupData-method}

\title{
Return the First or Last Part of the Species Matrix
}
\description{
Returns the first or last parts of the species matrix.
}
\usage{
head(x, n, choice, mode, ...) 
tail(x, ...)
}
% n = 6L
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object inheriting from class \code{VegsoupData}.
}
  \item{n}{
A single integer. If positive, size for the resulting object. If negative, all but the n last/first number of elements of x.
}
  \item{choice}{
Display first row of species matrix (the default) or sites data frame.
}
  \item{mode}{
If choice is \dQuote{species} of which mode should the returned matrix be reported. If missing the presence/absence mode of the species matrix is printed. See \code{\link{as.logical}}, \code{\link{as.numeric}}, and \code{\link{as.character}}, respectively.
}
  \item{\dots}{
not used
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object like \code{as.character(x)} but generally smaller.
\author{
Roland Kaiser
}

\seealso{
See \code{\link{head}} and \code{\link{tail}} in package \pkg{utils}.
}
\examples{
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet")))
head(dta)
}
\keyword{methods}

