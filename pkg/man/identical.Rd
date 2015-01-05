\name{identical}

\docType{methods}

\alias{identical}
\alias{identical,Sites,Species-method}
\alias{identical,Species,Sites-method}

\title{ 
Intersect plot names
}

\description{
Intersects Species and Sites objects if they have matching plots. Returns \code{TRUE} if both objects match.
}

\usage{
\S4method{identical}{Sites,Species}(x, y)

\S4method{identical}{Species,Sites}(x, y)
}

\arguments{
  \item{ x }{Species object}
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

%\seealso{
%}

%\examples{
%}

\keyword{methods}

