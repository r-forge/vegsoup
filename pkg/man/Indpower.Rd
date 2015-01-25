\encoding{UTF-8}

\name{Indpower}
\docType{methods}

\alias{Indpower}
\alias{Indpower,Vegsoup-method}

\title{ Indicator Power of Species }
\description{
Indicator power calculation of Halme et al. (2009) or the congruence between indicator and target species.
}
\usage{
\S4method{Indpower}{Vegsoup}(obj, type, ...)
}

\arguments{
  \item{ obj }{
	A \code{Vegsoup*} object.
  }
  \item{ type }{
	Nothing here yet.
  }
  \item{ \dots }{
	arguments passed to \code{vegan::indpower} to specify \code{type}.
  }
}

\details{
  See \code{\link[vegan:indpower]{indpower}} for details.
}

\value{
 \code{"matrix"}.
}

\references{
Halme, P., M"onkk"onen, M., Kotiaho, J. S, 
Ylisirni"o, A-L. 2009. Quantifying the indicator power
of an indicator species. Conservation Biology 23: 1008-1016.
}

\author{
Interfaced from Roland Kaiser for package vegsoup from function
\code{indpower} by \emph{Peter Solymos} in package \pkg{vegan}.
}

\seealso{
\code{\link{Indspc}}
}

\keyword{ methods }

