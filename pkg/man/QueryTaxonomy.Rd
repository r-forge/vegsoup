\name{QueryTaxonomy}
\alias{QueryTaxonomy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Query taxonomy for raw csv files.
}
\description{
Given a valid csv file representing species abundances, query the used abbreviation (column abbr) against a taxonomic reference list and create an translating taxa abbreviations to taxon names.  
}
\usage{
QueryTaxonomy(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
filename of species csv-file. Should have a column named 'abbr' .
}
  \item{y}{
filename of taxonomy csv-file. Should have columns named 'abbr' and 'taxon'.
}
}
\details{
All unique abbreviations found in x are matched against y. Any non matching items are printed on the screen.
}
\value{
Returns an object of class \code{"data.frame"} with columns 'abbr' and 'taxon'.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser
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
