\name{VegsoupDataPartition}
\alias{VegsoupDataPartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do
}
\description{
Create an object of class VegsoupDataPartition
}
\usage{
VegsoupDataPartition(x, binary, k, method = c("ward", "flexible", "pam", "isopam", "kmeans", "optpart", "wards", "external"), dist = "bray", clustering, polish = FALSE, nitr = 999, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object of class VegsoupData}
  \item{binary}{use presence/absence only}
  \item{k}{number of partitions to create}
  \item{method}{clustering method, see details}
  \item{dist}{a distance measure known by \link{vegdist}}
  \item{clustering}{external vector of cluster partitons}
  \item{polish}{apply optimal silhouette widths clustering \link{opts} on possible outgroups (single element partitions) to sweep them to an existing multi element partition}
  \item{nitr}{number of iterations of function \link{tabbed} in calculating table deviance}
  \item{verbose}{set to \code{TRUE} for verbose output}
  \item{\dots}{passed to methods}
}
\details{
Parttion an object of class VegsoupData using several clustering methods.
}
\value{
\code{VegsoupDataPartition} returns an object of class \code{VegsoupDataPartition}
}
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
%%\code{\link{VegsoupDataPartition-class}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
