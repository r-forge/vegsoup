\name{identical}

\docType{methods}

\alias{identical}

\alias{identical,Sites,Species-method}
\alias{identical,Species,Sites-method}
\alias{identical,SpeciesTaxonomy,Sites-method}
\alias{identical,Species,Taxonomy-method}

\title{ 
Intersect plot names
}

\description{
Intersects Species, Sites or Taxonomy objects if they have matching plots or abbreviations. Returns \code{TRUE} if both objects match.
}

\usage{
\S4method{identical}{Sites,Species}(x, y)

\S4method{identical}{Species,Sites}(x, y)

\S4method{identical}{SpeciesTaxonomy,Sites}(x, y)

\S4method{identical}{Species,Taxonomy}(x, y)
}

\arguments{
  \item{ x }{Species or SpeciesTaxonomy object}
  \item{ y }{Sites object}
%  \item{ num.eq } TRUE
%  \item{ single.NA } TRUE
%  \item{ attrib.as.set} TRUE
%  \item{ ignore.bytecode} TRUE
% \item{ ignore.environment} FALSE
} 

\value{
logical.
}

\seealso{
\code{\link{intersect}}
}

%\examples{
%}

\keyword{methods}

