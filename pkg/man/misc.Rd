\encoding{UTF-8}

\name{miscellaneous functions}
\alias{compass}
\alias{singleton}
\alias{singletons}

\title{ Utility Functions }

\description{
  Simple functions for regular data precessing tasks.
}

\usage{
compass(x, fine = TRUE)

singleton(obj)

singletons(obj)
}

\arguments{
  \item{ x }{
	numeric.
}
  \item{ fine }{
	logical.
}
  \item{ obj }{
	\code{'VegsoupPartition'} object.
}

}

\details{
  Function \code{compass} cuts numeric values of angular degrees into characters
  of the wind-rose. If called without arguments the functions reports the
  breakpoints an associated labels.
  \code{singeltons} is \code{TRUE} if any partition has only 1 member;
  \code{singelton} returns the index.

}

\value{
character (compass)
numeric (singelton)
logical (singeltons)
}

\author{ Roland Kaiser }

\examples{
compass(c(0,220))
compass(22, fine = FALSE)
compass(22, fine = TRUE)
}

\keyword{ spatial }

