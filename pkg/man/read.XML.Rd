\encoding{UTF-8}

\name{verbatim}

\alias{read.XML}

\title{ Read From TurboVeg XML Files }

\description{
  The base S3 function \code{read.XML(file)} reads data from XML files exported
  from the turboveg data base (Hennekens \& Joop 2009).
}

\usage{
read.XML(file)
}

\arguments{
  \item{ file }{
    character. Path to a plain text file (text array).
  }
}

\details{
The way \emph{turboveg} exports to XML is very similar to the data model of a Vegsoup object.
}

\value{
\code{Vegsoup} object
}

\author{
Roland Kaiser
}
\references{
Hennekens, S M and Joop, H J. 2009. Turboveg, a comprehensive data base management system for vegetation data. Journal of Vegetation Science, 12:589–591.
}
\examples{
file <- system.file("extdata", "turboveg.xml", package = "vegsoup")
x <- read.XML(file)
#	assign coordiantes
coordinates(x) <-  ~ e_coord + n_coord
}
\seealso{
\code{\link{read.verabatim}}, \code{\link{plainsoup}} and \code{\link{instantsoup}}
}
\keyword{ import }

