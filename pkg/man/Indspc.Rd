\encoding{UTF-8}

\name{compspec}
\docType{methods}

\alias{compspec}
\alias{compspec,Vegsoup-method}

\title{ Compositional Indicator Species Analysis }

\description{
Calculates the mean similarity of all plots in which a species occurs.
}

\usage{
\S4method{compspec}{Vegsoup}(obj, method, ...)
}

\arguments{
  \item{ obj }{
	A \code{Vegsoup*} object.
  }
  \item{ method }{
   Nothing here yet.
  }
  \item{\dots}{
	arguments passed to \code{labdsv::indspc} to specify \code{numitr}.
  }
}

\details{
  See \code{\link[labdsv:indspc]{indspc}} for details.
}

\value{
  An object of class \code{"indspc"}. See \code{\link[labdsv:indspc]{indspc}}
  in package \pkg{labdsv}.
}

\author{
Interfaced from Roland Kaiser for package vegsoup from function
\code{indspc} by \emph{David. W. Roberts} in package \pkg{labdsv}.
}

\seealso{
\code{\link{fidelity}}
}

\examples{
require(vegsoup)

data(barmstein)

compspec(barmstein)
}
\keyword{ methods }

