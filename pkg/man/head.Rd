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
head(x, n, choice, typeof, ...)

tail(x, ...)
}
\arguments{

  \item{ x }{
An object inheriting from class \code{VegsoupData}.
}
  \item{ n }{
A single integer. If positive, size for the resulting object. If negative, all but the n last/first number of elements of x. defaults to \code{n = 6L}.
}
  \item{ choice }{
Display first row of species matrix (the default) or sites data frame.
}
  \item{ typeof }{
If choice is \dQuote{species} of which mode should the returned matrix be reported (see \code{\link{as.matrix}}). If missing a presence/absence species matrix is printed (\code{typeof = "logical"}).
}
  \item{ \dots }{
Passed to functions.
}
}

\section{Methods}{

\describe{
\item{ \code{signature(obj = "VegsoupData")} }{
\code{head()} (\code{tail()}) returns the first (last) \code{n} rows of the \emph{community matrix} when \code{choice = "species"} and the \code{n} rows of the \emph{sites data frame}.
}
\item{ \code{signature(obj = "VegsoupDataPartition")} }{
If \code{n != 6L} the method prints the \emph{typal} (\code{\link{typal}}) sample for each partition, with the number of rows equaling \code{getK(x)}. If \code{n} is left as the default the behaviour is the same as defined for class \code{VegsoupData}.
}
}

}

\value{
An object like \code{as.matrix(x)} but generally smaller.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{as.matrix}}, \code{\link{typal}} in package \pkg{vegsoup}

\code{\link[utils:head]{head}} and \code{\link[tail:head]{tail}} in package \pkg{utils}.
}
\examples{
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
		scale = list(scale = "Braun-Blanquet")))
head(dta)

head(dta, "sp") # choice can abbreviated
head(dta, "sp", "numeric") # choice can abbreviated

prt <- VegsoupDataPartition(dta, k = 2)

t(head(prt, n = -1))
}
\keyword{methods}


