\name{Layer}
\alias{Layer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return layer for each observation
}
\description{
Simply return the layer for each observation.
}
\usage{
Layer(obj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ a class inheriting from class \code{vegsoup}}
  \item{\dots}{ passed from to methods}
}
\details{
Simply return the layer for each observation (\code{SpeciesLong(obj)$layer}).
}
\value{\code{Layer} returns a character vector equal in length to \code{nrow(SpeciesLong(obj)}}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Roland Kaiser (\email{kardinal.eros@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Vegsoup-class}
}
\examples{
#	forest <- unique(SpeciesLong(dta)[Layer(dta) == "tl1",]$plot)
#	forest <- dta[match(forest, rownames(dta)), ]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

