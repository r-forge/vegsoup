\encoding{UTF-8}

\name{compress}
\alias{compress}
\alias{compress,Vegsoup-method}

\title{ Compress Object and Retain Minimal Information }

\description{
  Set \code{Coverscale("pa")}, reduce columns of sites, collapse all layers
  (if present), and reduce content of taxonomy to only the mandatory columns.
}

\usage{
\S4method{compress}{Vegsoup}(x, retain)
}

\arguments{
  \item{ x }{
   \code{Vegsoup*} object.
  }
  \item{ retain }{
   character. Column(s) to retain
  }
}

\details{
  If present, the method tries to retain columns \code{family} and \code{level}
  from \code{taxonomy(x)} and it will otherwise return just the mandatory columns \code{abbr} and \code{taxon}.
}

\value{
  returns an object depending on the input class.
}

%\references{
%}

\author{ Roland Kaiser }

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

compress(x)
compress(x, retain = c("date", "cov"))

}
\seealso{
\code{\linkS4class{Coverscale}},
\code{\link{coverscale}}
}
\keyword{ methods }
