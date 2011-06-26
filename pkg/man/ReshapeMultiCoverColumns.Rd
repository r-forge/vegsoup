\name{ReshapeMultiCoverColumns}
\alias{ReshapeMultiCoverColumns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape raw csv-files where abundance values are in seperate columns.
}
\description{
Utility function to translate some commonly used data models to store species data.
}
\usage{
ReshapeMultiCoverColumns(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
filename of csv-file to translate.
}
}
\details{
Currently there is no user interface to select colums from raw csv-files.
}
\value{
Returns an object of class \code{"data.frame"} with columns 'plot', 'abbr', 'layer' and 'cove'.
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
