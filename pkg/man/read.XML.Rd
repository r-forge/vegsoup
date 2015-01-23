\encoding{UTF-8}

\name{ Turboveg }

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
The function tries to format the date field that is in the \emph{standard_record} tag.
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
\code{\link{read.verbatim}}, \code{\link{plainsoup}} and \code{\link{instantsoup}}
}
\keyword{ import }

