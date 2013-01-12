\name{validate}
\docType{methods}
\alias{Tabdev}
\alias{Tabdev-methods}
\alias{Tabdev,VegsoupDataPartition-method}
\alias{Partana}
\alias{Partana-methods}
\alias{Partana,VegsoupDataPartition-method}

\title{
Partition Quality/Validity Assessment of Objects of Class VegsoupDataPartition
}
\description{
by Table Deviance
}
\usage{
Tabdev(obj, numitr = 999, verbose = FALSE, ...)
Partana(obj, verbose = FALSE, ...)
}

\arguments{
  \item{obj}{
An object of class \code{"VegsoupDataPartition"}
}
  \item{ numitr }{
Number of iterations to perform in calculating the probability of obtaining as effective a classification as observed.
}
  \item{ verbose }{
} 
  \item{\dots}{
Arguments passed to \code{\link{as.dist}} (\code{Partana}) or \code{link{as.matrix}} (\code{Tabdev}).
}
}
\details{
\code{Partana} (\emph{partition analysis}) evaluates the within-cluster to among-cluster similarity of partitions as a measure of cluster validity. See \code{\link[optpart:partana]{partana}} for details.

As opposed to \emph{partition analysis} (\code{Partana}) \code{Tabdev} is a method to assess the quality of partitions with respect to the original data, and not its distance matrix representation. See \code{\link[optpart:tabdev]{tabdev}} for details.

}
\value{
\code{Partana} returns an S3 object of class \code{"partana"}.

\code{Tabdev} returns a \code{data.frame} with four columns giving
the species (\code{"numocc"}),
the number of occurences of a species in the data set (\code{"numocc"}),
estimated deviance \code{"deviance"},
and probability (\code{pval}).

%% \item{ spcdev }{
%% }
%% \item{ totdev }{
%% }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Implemented for vegsoup using Dave Roberts \code{tabdev} and \code{partana} algorithms.
}

\seealso{
\code{\link{Optsil}},
\code{\link{Optindval}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
