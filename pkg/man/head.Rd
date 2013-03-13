\name{head}

\alias{head}
\alias{head-methods}
\alias{head,Vegsoup-method}
\alias{head,VegsoupPartition-method}
\alias{tail}
\alias{tail-methods}
\alias{tail,Vegsoup-method}

\title{
Return the First or Last Part of the Species Matrix
}
\description{
Returns the first or last parts of the species matrix.
}
\usage{
head(x, n = 6L, choice, typeof, ...)

tail(x, n = 6L, choice, typeof, ...)
}
\arguments{

  \item{ x }{
    An object inheriting from class \code{Vegsoup}.
  }
  \item{ n }{
    A single integer. If positive, size for the resulting object. If negative,
    all but the n last/first number of elements of x. defaults to \code{n =
    6L}.
  }
  \item{ choice }{
    Display the first rows of the species matrix (\code{"species"},
    the default), or the sites data frame (\code{choice = "sites"}).
  }
  \item{ typeof }{
    If choice is \dQuote{species} of which mode should the returned matrix be
    reported (see \code{\link{as.matrix}}). If missing a presence/absence
    species matrix is printed (\code{typeof = "logical"}).
  }
  \item{ \dots }{
    Passed to functions.
  }
}

\section{Methods}{

  \describe{
    \item{ \code{signature(obj = "Vegsoup")} }{
      \code{head()} (\code{tail()}) returns the first (last) \code{n} rows of
      the \emph{community matrix} when \code{choice = "species"} and the
      \code{n} rows of the \emph{sites data frame}.
    }
    \item{ \code{signature(obj = "VegsoupPartition")} }{
      If \code{n != 6L} the method prints the \emph{typal}
      (\code{\link{typal}}) sample for each partition, with the number of rows
      equaling \code{getK(x)}. If \code{n} is left as the default the
      behaviour is the same as defined for class \code{Vegsoup}.
    }
  }

}
\note{
  Call the method with named arguments (\code{'choice'} respectively
  \code{'typeof'}). See \sQuote{Examples}.
}
\value{
  An object like \code{as.matrix(x)} but generally smaller.
}
\author{ Roland Kaiser }
\seealso{
\code{\link{as.matrix}} and \code{\link{typal}} in package \pkg{vegsoup}

\code{\link[utils:head]{head}} and \code{\link[utils:tail]{tail}} in package \pkg{utils}.
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, coverscale = "braun.blanquet")

# print sites or species
head(dta, choice = "sites")
head(dta, choice = "sp") # choice can abbreviated

head(dta, "si", n = 3) 
tail(dta, choice = "sp", typeof = "character", n = 2)

prt <- VegsoupPartition(dta, k = 2)

head(prt)
}
\keyword{methods}
