\name{Extract}
%% was $-methods
%%\docType{methods}
\alias{Extract}
\alias{[-methods}
\title{Extract or Replace Parts of the Sites Data Slot of Objects inheriting VegsoupData}
\description{
Extract or replace parts of the sites data.
}

\usage{
## S4 method for signature 'Vegsoup*'
x[i, j, ... , drop = FALSE]
}
\arguments{
  \item{ x }{
    An object ingeriting from class \code{VegsoupData}.
  }
  \item{ i, j }{
    Indices specifying elements to extract or replace.
  }
  \item{ name }{
    A character string or a name.
  }
}
\details{
If only one plot is requested for indexing (e.g. \code{obj[1,]}) the \code{summary} and \code{show} methods additionally will print a species list.
}
\note{
When objects of class \code{"VegsoupDataPartition"} are subsetted it is likely that the partitioning vector (\code{Partitioning(obj)}) has to be subsetted too. If this is the case, the number of partitions \code{"k"} (\code{getK(obj)}) is changed (lowered) accordingly.
}
\examples{
require(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# summary returns a species list
dta[1,]

#	do.call("$", list(dta, "variable"))   
} 

%%}}
