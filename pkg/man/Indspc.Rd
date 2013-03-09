\encoding{UTF-8}
\name{Indspc}
\docType{methods}
\alias{Indspc}
\alias{Indspc-methods}
\alias{Indspc,VegsoupData-method}

\title{ Compositional Indicator Species Analysis }
\description{
Calculates the mean similarity of all plots in which a species occurs.
}
\usage{
Indspc(obj, ...)
}
\arguments{
  \item{ obj }{
    A \code{Vegsoup*} object.
  }
  \item{ \dots }{
    arguments passed to \code{labdsv::indspc} to specify \code{numitr}.
  }
}
\details{
  See \code{\link[labdsv:indspc]{indspc}} for details.
}
\value{
  An object of class \code{"indspc"}.
}
\author{
Interfaced from Roland Kaiser for package vegsoup from function
\code{indspc} by \emph{David. W. Roberts} in package \pkg{labdsv}.
}
\seealso{
\code{\link{Fidelity}}
}
\keyword{ methods }
