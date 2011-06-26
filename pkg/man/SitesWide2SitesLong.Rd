\name{SitesWide2SitesLong}
\alias{SitesWide2SitesLong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stacks raw csv-files in wide format. 
}
\description{
Stack a plot by measurement matrix to long format, with columns 'plot', 'variable' and 'value'.
}
\usage{
SitesWide2SitesLong(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
filename of csv-file to stack.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an object of class \code{"data.frame"} with columns 'plot', 'variable' and 'value'.
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
\code{\link{VegsoupData-class}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
