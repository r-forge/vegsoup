\name{testdata}

\alias{testdata}
\alias{spc}
\alias{sts}
\alias{txn}
\alias{barmstein}

\docType{data}

\title{ Test Data Set }

\description{
Simple data structures for testing. A species and corresponding taxonomy data.frame plus a sites data.
}

\format{
The three objects \code{"spc"} (species), \code{"sts"} (sites) and \code{"txn"} (taxonomy) each contain the following columns:
\describe{
  \strong{spc:}
    \item{plot}{plot identifier, mode \code{"character"}}
    \item{abbr}{species names, mode \code{"character"}}
    \item{layer}{layer code, mode \code{"character"}}
    \item{cov}{species abundance, mode \code{"character"}}

  \strong{sts:}
    \item{plot}{plot identifier, mode \code{"character"}}
    \item{variable}{name of the variable, mode \code{"character"}}
    \item{value}{value for each variable, mode \code{"character"}}

  \strong{txn:}
    \item{abbr}{species identifier, mode \code{"character"}}
    \item{taxon}{scientific species name, mode \code{"character"}}
}
}

\usage{
data(barmstein)
data(testdata)
}

\examples{
data(testdata)
}
