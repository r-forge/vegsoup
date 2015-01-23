\encoding{UTF-8}

\name{colSums}
\docType{methods}

\alias{colSums}
\alias{colSums,Vegsoup-method}

\alias{rowSums}
\alias{rowSums,Vegsoup-method}

\alias{rowSums}
\alias{rowSums,Vegsoup-method}

\alias{rowMeans}
\alias{rowMeans,Vegsoup-method}

\alias{colMeans}
\alias{colMeans,Vegsoup-method}

\title{ Row and Column Sums for Objects of Class VegsoupData* }

\description{
Form row and column sums for the the species matrix based on presence/absence and numeric matrices.
}

\usage{
\S4method{colSums}{Vegsoup}(x, na.rm = FALSE, dims = 1, typeof = "logical", ...)

\S4method{rowSums}{Vegsoup}(x, na.rm = FALSE, dims = 1, typeof = "logical", ...)

\S4method{colMeans}{Vegsoup}(x, na.rm = FALSE, dims = 1, typeof = "numeric", ...)

\S4method{rowMeans}{Vegsoup}(x, na.rm = FALSE, dims = 1, typeof  = "numeric", ...)
}

\arguments{
  \item{ x }{
    An \code{Vegsoup*} object.
  }
  \item{ na.rm }{
    logical. Should missing values (including NaN) be omitted from the
    calculations?
  }
  \item{ dims }{
    Not used.
  }
  \item{ typeof }{
    The mode of the species matrix to compute values from, defaults to
    \code{"logical"} for \code{rowSums} and \code{colSums}. See
    \code{\link{as.matrix}} for details about argument \code{'typeof'}. The
    \code{rowMeans} and \code{colMeans} methods use \code{typeof = "numeric"}
    as the default and \code{typeof = "character"} is not supported by neither
    of the functions.
  }
  \item{ \dots }{
    Arguments passed to \code{\link[base:rowSums]{rowSums}},
    \code{\link[base:rowSums]{rowSums}}, \code{\link[base:colSums]{colSums}}
    and \code{\link[base:colMeans]{colMeans}}, respectively.
  }
}

\details{
Note, the default behaviour (argument \code{typeof} is missing) operates on a presence/absence version of the species matrix as can be obtained by \code{as.logical(x)} or \code{as.matrix(x, typeof = "logical")}. On this basis \code{colSums} returns the number of occurences of a species in the data set. The \code{rowSums} method returns the number of species with duplicated occurences of species on different layers if there is more than one layer and at least a single species occuring in more than one layer! In this case correct species richness can be obtained with method \code{richness(obj, "sample")}.

The \code{colSums}
}

\value{
A numeric vector of suitable size with names taken from the object.
}

\author{ Roland Kaiser }

\seealso{
\code{\link[base:colSums]{colSums}} in package \pkg{base}, \code{\link{as.matrix}} in package \pkg{vegsoup}, \code{\link{richness}}, \code{\link{layers}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# number of species per plot including layer replication
# default method defines typeof = "logical"
rowSums(x)

#   true species richness
richness(x, "sample")

#   sums of abundances per plot according to coverscale(x)
rowSums(x, typeof = "numeric")

# number of occurences for each species (typeof = "logical")
colSums(x)

# sums of abundances for each species including layer replication
colSums(x, typeof = "numeric")

# mean abundances, also with layer replication!
m1 <- colMeans(x)

# combine all strata
x0 <- layers(x, collapse = "NA") # note, collapse = NA will not work!
m2 <- colMeans(x0, typeof = "numeric")

m1[ grep("amel.oval", names(m1)) ]
m2[ grep("amel.oval", names(m2)) ]
}

\keyword{methods}

