\encoding{UTF-8}

\name{tables}
\docType{methods}

\alias{contingency}
\alias{contingency,Vegsoup-method}
\alias{contingency,VegsoupPartition-method}

\alias{constancy}
\alias{constancy,Vegsoup-method}
\alias{constancy,VegsoupPartition-method}

\alias{average}
\alias{average,VegsoupPartition-method}

\alias{as.table}
\alias{as.table,VegsoupPartition-method}

\title{ Constancy and Contingency Tables }

\description{
  Method \code{constancy} lists for each species the fraction (percentage) of
  samples in each partition (or the whole data set) the species occurs
  \code{contingency} lists just the number of occurrences. The latter is based
  on presence/absence of a species (layer-replicate), whereas method
  \code{average} divides the sum of species abundances by the number of plots in
  which it occurs. A tabulation of \code{partitioning(obj)} against a
  cross-classifying factor can be obtained with the \code{as.table} method.
  The grouping is taken from a data column of \code{sites(obj)}.
}

\usage{
\S4method{constancy}{Vegsoup}(obj, percentage = TRUE, ...)

\S4method{constancy}{VegsoupPartition}(obj, percentage = TRUE, ...)

\S4method{contingency}{Vegsoup}(obj, ...)

\S4method{contingency}{VegsoupPartition}(obj, ...)

\S4method{average}{VegsoupPartition}(obj, ...)

\S4method{as.table}{VegsoupPartition}(x, ...)
}

\arguments{
  \item{ obj, x }{
    \verb{Vegsoup} or \verb{VegsoupPartition} object.
  }
  \item{ percentage }{
    \verb{numeric}. Return results of \code{constancy} as percentages.
  }
  \item{ \dots }{
    additional arguments passed to methods. For \code{contingency} and
    \code{average} \verb{...} expands to arguments passed to matrix methods
    \code{as.logical} and \code{as.matrix} respectively (see
    \sQuote{Details}). Additional arguments to \code{constancy} are passed to
    \code{\link{round}} (the \code{digits} argument) and are only applicable
    if \code{percentage = TRUE}. For \code{as.table} \verb{...} expands to a
    character giving a column name of \code{sites(x)} or a vector which can be
    interpreted as factor.
  }
}

\details{
An importance table as returned by method \code{average} lists for each species the average (mean) abundance of each species (stratum-replicate) in each partition.  
Sensible results are only to be obtained if the object has either a semi-quantitative or quantitative cover scale defined (check \code{is.ordinal(obj)}).  
Note, if only presence/absence data is available \code{average} returns the same results as [\dots], which bears no information as the summed abundance is just the number of occurrences.  
See method \code{\link{quantile}} for descriptive summaries of species abundances in partitions.  

Specifying \code{mode = "R"} in \code{contingency} and \code{average} allows for a kind of \emph{R-mode} analysis where the grouping is defined by strata (\code{\link{layers}}).  
Note, this rarely makes sense if there is only one layer.  
For multi layer (stratum) objects (\code{length(layers(obj)) > 1}) a call to \code{average} in \emph{R-mode} implies that all species abundances in a layer are summed up and divided by layer contingencies (\code{contingency(obj, mode = "R")}), in this case the number of species is found in a respective layer for each plot.
}

\value{
For \code{constancy}, \code{contingency}, and \code{average} a object of class \code{'matrix'} with as many columns as there are partitions (strata if \code{mode = "R"}) and as many rows as species (respectively plots). If mode is not specified, strata-replicates are treated separately.

For \code{as.table} an object of class \code{'table'} with the levels of the cross-classifying factor in rows.
}

\author{ Roland Kaiser }

\seealso{
\code{\link[labdsv:const]{const}} and
\code{\link[labdsv:importance]{importance}} in \pkg{labdsv},
\code{\link{quantile}}
}

\examples{
data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

# constancy table for two partitions
head(constancy(x, digits = 1))

# equivalent
head(round(contingency(x) / as.vector(table(partitioning(x))) * 100, digits = 1))

# R-mode analysis
# remove presence/absence standardisation
decostand(x) <- NULL
contingency(x, mode = "R")
average(x, mode = "R")

# tabulate partitioning vector againts sites column
as.table(x, "expo")

# tabulate partitiong vector againts other vector
y <- rownames(subset(x, "Achnatherum")) # plot names with this species
y <- y == rownames(x)                   # TRUE where the species occurs

as.table(x, y)
}
\keyword{ methods }
