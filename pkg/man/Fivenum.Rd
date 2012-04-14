\name{Fivenum}
\alias{Fivenum}

\title{
Tukey Five-Number Summaries for 
}

\description{
Returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum) for each species in each cluster.
}

\usage{
Fivenum(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{an object of inheriting from class \code{\link{VegsoupDataPartition}}
}
}

\details{
There is special behavior of argument \code{na.rm = TRUE} that is used to handle missing observations of species in the community matrix. Those absences are conveniently coded as zeros. If \code{na.rm = TRUE} all zeros are replaced by \code{NA} and the function returns \code{NA} for absence of a particular species in a respective cluster. If \code{na.rm = FALSE} zeros are treated as is. If the input object has layers the summary statistics are calculated for each layer. If abundances are ordinal, \code{AbundanceScale(obj)$lims} is used to obtain numeric values for calculation. Note, suppling presence/absence data is supported, however, results may be meaningless.
}
\value{
An array with 5 dimensions containing the summary information of \code{\link{fivenum}} for a species by cluster matrix.
}

\author{
Roland Kaiser
}

\seealso{
\code{\link{boxplot.stats}},\code{\link{fivenum}},\code{\link{AbundanceScale}}.
}
\examples{
#	get minmum

#res <- Fivenum(dta)
#res[,,"min"]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
