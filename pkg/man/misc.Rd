\encoding{UTF-8}

\name{miscellaneous functions}
\alias{compass}

\title{ Utility functions }

\description{
Simple minded functions for regular data precessing tasks.
}

\usage{
compass(x)

singleon(obj)
}

\arguments{
  \item{ x }{
numeric.
}
  \item{ obj }{
VegsoupPartition object.
}

}

\details{
Function \code{compass} cuts numeric values of angular degrees into characters of the wind-rose.
\code{singeltons} is \code{TRUE} if any partition has only 1 member; \code{singelton} returns the index.
}

\value{
character (compass)
numeric (singelton)
logical (singeltons)
}

\author{ Roland Kaiser }

\examples{
compass(c(0,220))
}

\keyword{ spatial }

