\name{intersect}

\docType{methods}

\alias{intersect}

\alias{intersect,Sites,Species-method}
\alias{intersect,Species,Sites-method}
\alias{intersect,SpeciesTaxonomy,Sites-method}
\alias{intersect,Species,Taxonomy-method}

\title{ 
Intersect plot names
}

\description{
Intersects Species*, Sites or Taxonomy objects if they have matching plots or abbreviation. Returns \code{TRUE} if both objects match.
}

\usage{
\S4method{intersect}{Sites,Species}(x, y)

\S4method{intersect}{Species,Sites}(x, y)

\S4method{intersect}{SpeciesTaxonomy,Sites}(x, y)

\S4method{intersect}{Species,Taxonomy}(x, y)
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
\code{\link{identical}}
}

%\examples{
%}

\keyword{methods}

