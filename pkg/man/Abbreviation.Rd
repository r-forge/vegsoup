\name{Abbreviation}
\docType{methods}
\alias{Abbreviation}
\alias{Abbreviation-methods}
\alias{Abbreviation,Vegsoup-method}
\alias{Abbreviation,VegsoupData-method}
\alias{Abbreviation<-}
\alias{Abbreviation<--methods}
\alias{Abbreviation<-,Vegsoup,character-method}

\title{
Get Abbreviations used to Encode Species
}
\description{
Get the abbreviations used in species matrix and corresponding taxonomy data.frame.
}
\usage{
Abbreviation(obj, ...)
Abbreviation(obj) <- value
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
Abbreviation(dta)
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}


\keyword{method}
\keyword{classes}% __ONLY ONE__ keyword per line
