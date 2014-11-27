\encoding{UTF-8}

\name{compress}
\alias{compress}
\alias{compress,Vegsoup-method}

\title{ Compress object and retain minimal information }

\description{
Sets \code{Coverscale("pa")} and reduces columns of sites.
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

%\details{
%}

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

