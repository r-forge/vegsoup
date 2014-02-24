\name{tables}
\docType{methods}

\alias{contingency}
\alias{contingency,VegsoupPartition-method}

\alias{constancy}
\alias{constancy,VegsoupPartition-method}

\alias{average}
\alias{average,VegsoupPartition-method}

\title{ Summary Tables for \code{"VegsoupPartition"} Objects }

\description{
Function \code{constancy} lists for each species the fraction (percentage) of samples in each partition the species occurs in; \code{contingency} lists just the number of occurrences. The latter are based on presence/absence of a species, whereas method \code{average} divides the sum of species abundances by the number of plots in which it occurs.
}

\usage{
\S4method{contingency}{VegsoupPartition}(obj, ...)

\S4method{constancy}{VegsoupPartition}(obj, percentage = TRUE, ...)

\S4method{average}{VegsoupPartition}(obj, ...)
}

\arguments{
  \item{ obj }{
    \code{'VegsoupPartition'} object.
  }
  \item{ percentage }{
    Return results of \code{constancy} as percentages, the default.
  }
  \item{ \dots }{
    For \code{contingency} and \code{average} \dots expand to arguments
    passed to matrix methods \code{as.logical} and \code{as.matrix}
    respectively (see \sQuote{Details}). Additional arguments to
    \code{constancy} are passed to \code{\link{round}} and are only applicable
    if \code{percentage = TRUE}, the default.
  }
}

\details{
An importance table lists for each species the average (mean) abundance of each species in each partition. Sensible results are only to be obtained if the object has either a semi-quantitative or quantitative cover cover scale defined. Note, if only presence/absence data is available \code{importance} returns the same results as \dots \dots, which bears no information as the summed abundance is just the number of occurrences. See method \code{\link{quantile}} for descriptive summaries of species abundances in partitions.

Specifying \code{mode = "R"} in \code{Contingency} and \code{Importance} allows for a kind of \emph{R-mode} analysis where the grouping is defined by the layer structure (\code{\link{Layers}}). Note, this rarely makes sense if there is only one layer. For multi layer objects a call to \code{Importance} in \emph{R-mode} implies that all species abundances in a layer are summed up and divided by layer contingencies (\code{Contingency(obj, mode = "R")}), in this case the number of species found in a respective layer for each plot.
}

\value{
A \code{"matrix"} with as many columns as there are partitions (layers if \code{mode = "R"}) and as many rows as species (or plots). If mode is not specified layer pseudo-species are treated separately.
}

\author{ Roland Kaiser }

\seealso{
\code{\link[labdsv:const]{const}} and
\code{\link[labdsv:importance]{importance}} in \pkg{labdsv},
\code{\link{quantile}}
}

\examples{
require(vegsoup)

data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

# constancy table for two partitions
head(constancy(x))

# equivalent
head(contingency(x) / as.vector(table(Partitioning(x)))) * 100

# R-mode analysis
# remove presence/absence standardisation
decostand(x) <- NULL
contingency(x, mode = "R")
average(x, mode = "R")
}
\keyword{methods }
