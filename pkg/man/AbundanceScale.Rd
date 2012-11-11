\name{AbundanceScale}
\alias{AbundanceScale}
\title{
Retrieves the Abundance Scale
}
\description{
Retrieves abundance scale from \code{slot(obj, "scale")} of object. A list with three components \code{scale}, \code{codes} and \code{lims} (see value).
}
\usage{
AbundanceScale(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
an object inheriting from class \code{Vegsoup} 
}
}
\details{
Predefined abundabce scales supplied by function \code{\link{Vegsoup}} include "Braun-Blanquet", "Braun-Blanquet 2",  "Domin", "frequency" and "binary". 
}
\value{
a list with three components
	\item{scale}{the name refered to the abunace scale}
	\item{codes}{if scale is treated as ordinal, a vector of mode \code{"character"} giving the labels of the scale}
	\item{lims}{mean percentage cover corresponding to \code{codes}}
%% ...
}
\references{
Shimwell, D.W. (1971). The Description and Classification of Vegetation. Sidgwick & Jackson.
}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BraunBlanquetReduce}}, \code{\link{Vegsoup}}
}

\examples{
library(vegsoup)

data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
AbundanceScale(dta)}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
