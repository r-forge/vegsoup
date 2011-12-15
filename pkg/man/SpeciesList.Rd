\name{SpeciesList}
\alias{SpeciesList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species List
}
\description{
Return a species list of the data set.
}
\usage{
Specieslist(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{an object inheriting from \code{class("vegsoup")}}
  \item{layered}{if \code{TRUE} returns species list for each layered.}
}
\details{
Convinience method to return a unique species list from the data set. The same can be obtained using \code{Taxonomy(obj)}. If \code{layered = TRUE} returns a unique species list for each layer, if there is any, in the data set.
}
\value{ a two-column data.frame; the first column has the taxon abbreviations, the second the scientific taxon names. If \code{layered = TRUE} an additional column identifying the layer is returned.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser (\email{kardinal.eros@gmail.com})
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
