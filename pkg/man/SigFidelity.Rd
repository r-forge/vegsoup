\name{SigFidelity}
\docType{methods}
\alias{SigFidelity}
\alias{SigFidelity-methods}
\alias{SigFidelity,VegsoupDataPartition-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SigFidelity(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{ mode }{
type of analysis. See \sQuote{Details}.
}

  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{

\code{mode = 0} looks for species whose abundance is significantly higher in one of the groups. \code{mode = 0} looks for species whose abundance is significantly higher in sites belonging to one group as opposed to sites not belonging to it.
}
\value{
%An object of class \code{"VegsoupDataPartitionFidelity"}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Adapted from function \code{signassoc()} of package \pkg{indicspecies} by Miquel De Cáceres Ainsa.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
