\name{Abbreviation}
\alias{Abbreviation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Abbreviations used to Encode Species
}
\description{
Get the abbreviations used in species matrix and corresponding taxonomy data.frame.
}
\usage{
Abbreviation(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
an object inheriting from class \code{Vegsoup}
}
  \item{\dots}{
not used
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a vector of mode \code{"character"}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
}
\examples{
library(vegsoup)

data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))
Abbreviation(dta)}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{method}
\keyword{classes}% __ONLY ONE__ keyword per line
