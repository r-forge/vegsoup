\name{Fivenum}
\alias{Fivenum}

\title{
Tukeys Five-Number Summaries for a Community Matrix
}

\description{
Returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) for each species in each cluster.
}

\usage{
Fivenum(obj, na.rm = TRUE, recode = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{an object of inheriting from class \code{\link{VegsoupPartition}}
}
 \item{na.rm}{

}
 \item{recode}{recode abundances and return orginal scale. Only applies if scale if ordinal.
}
}

\details{
There is special behavior of argument \code{na.rm = TRUE} that is used to handle missing observations of species in the community matrix. Those absences are conveniently coded as zeros. If \code{na.rm = TRUE} all zeros are replaced by \code{NA} and the function returns \code{NA} for absence of a particular species in a respective cluster. If \code{na.rm = FALSE} zeros are treated as is. If the input object has layers the summary statistics are calculated for each layer. If abundances are ordinal \code{as.numeric(obj)} is used to obtain numeric values for calculation. If \code{recode = TRUE} results are returned in ordinal scale (e.g. Braun-Blanquet scale), otherwise as class midpoints as defiuned in \code{slot(obj, "scale")}. Note, suppling presence/absence data is supported, however, results may be meaningless.
}
\value{
An array with 5 dimensions containing the summary information of \code{\link{fivenum}} for a species by cluster matrix.
}

\author{
Roland Kaiser
}

\seealso{
\code{\link{boxplot.stats}}, \code{\link{fivenum}}, \code{\linkS4class{Coverscale}}.
}
\examples{
data(testdata)
#	get minmum

prt <- VegsoupPartition(Vegsoup(spc, sts, txa, "braun.blanquet"), k = 2)
res <- Fivenum(prt)
res[,,"max"]

res <- Fivenum(prt, recode = TRUE)
res[,,"max"]
}
\keyword{ univar }
