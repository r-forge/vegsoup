\encoding{UTF-8}

\name{Coverscale-class}
\Rdversion{1.1}
\docType{class}

\alias{Coverscale-class}
\alias{coerce,list,Coverscale-method}

\title{ Coverscale Class }

\description{
Contains the defintion for a cover scale (abundance scale) associated with an \code{Vegsoup*} object.
}
\section{Objects from the Class}{
  Objects can be created by calls to the function \code{\link{Coverscale}} or
  of the form \code{new("Coverscale", ...)}.

}
\section{Slots}{
  \describe{
	\item{\code{name}:}{A \code{character} object giving a descriptive name of the definition.}
	\item{\code{codes}:}{A \code{character} vector giving the level codes for ordinal scales, or \code{NULL}.}
	\item{\code{lims}:}{A \code{numeric} vector defining the mean percentage cover for each entry in \code{codes} or \code{NULL}.}
  }
}
\section{Methods}{
  \describe{
	\item{coerce}{
	  \code{signature(from = "list", to = "Coverscale")}:
	  The \code{from} object has to be a list of length three. Components
	  \code{[2:3]} can be \code{NULL}.
	}
  }
}
\references{
Peet, R.K. & Roberts, D.W. (2013). Classification of natural and semi-natural vegetation. In Vegetation Ecology (van der Maarel, E. & Franklin, J. eds). Wiley-Blackwell.

Barkman, J.J., Doing, H. & Segal, S. Kritische Bemerkungen und \enc{Vorschläge}{Vorschlaege} zur quantitativen Vegetationsanalyse. Acta Botanica Neerlandica, 13:394-419, 1964.

Certainly, more references are needed here!
}
\author{
Roland Kaiser
}

\seealso{
\code{\link{Coverscale}},
\code{\link{coverscale}}
}
\examples{
showClass("Coverscale")
Coverscale()
}
\keyword{classes}

