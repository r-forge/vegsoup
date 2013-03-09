\encoding{UTF-8}
\name{Indpower}
\docType{methods}
\alias{Indpower}
\alias{Indpower-methods}

\title{ Indicator Power of Species }
\description{
Indicator power calculation of Halme et al. (2009) or the congruence between indicator and target species.
}
\usage{
Indpower(obj, ...)
}

\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ \dots }{
    arguments passed to \code{vegan::indpower} to specify \code{type}.
  }
}

\details{
  See \code{\link[vegan:indpower]{indpower}} for details.
}
\value{
An object of class \code{"matrix"}.
}
\references{
Halme, P., \enc{Mönkkönen}{Monkkonen}, M., Kotiaho, J. S, 
\enc{Ylisirniö}{Ylisirnio}, A-L. 2009. Quantifying the indicator power
of an indicator species. \emph{Conservation Biology} 23: 1008--1016.
}
\author{
Interfaced from Roland Kaiser for package vegsoup from function
\code{indpower} by \emph{Peter Solymos} in package \pkg{vegan}.
}
\seealso{
\code{\link{Indspc}}
}
\keyword{ methods }
