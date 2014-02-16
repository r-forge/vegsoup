\name{miscellaneous functions}
\alias{compass}

\title{ Utility functions }

\description{
Simple minded functions for regular data precessing tasks.
}

\usage{
compass(x)
}

\arguments{
  \item{ x }{
numeric.
}
}

\details{
Function \code{compass} cuts numeric values of angular degrees into characters of the wind-rose.
}

\value{
character
}

\author{ Roland Kaiser }

\examples{
compass(c(0,220))
}

\keyword{ spatial }
