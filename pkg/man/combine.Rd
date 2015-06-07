\encoding{UTF-8}

\name{combine}
\alias{combine}

\title{ Combine taxa }

\description{
Combine two or more taxa by selecting scientific species names.
}

\usage{
combine(x, y, z)
}

\arguments{
  \item{ x }{
   \code{Vegsoup*} object.
  }
  \item{ y }{
  list.
  }
  \item{ z }{
   list.
  }
}

\details{
This utility function can be used to perform taxonomic aggregation.
Argument \code{y} defines the taxa to be combined (list element \code{from}) and a \emph{new} taxonomic description of that aggregation (list element \code{to}).
Argument \code{z} holds the \emph{new} abbreviation (see class \code{\linkS4class{Taxonomy}}) for the aggregated taxon and a taxonomic description of that aggregation (cp. argument \code{y}).
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

# display all taxa
taxon(x)

# define arguments
y <- list(from = c("Teucrium chamaedrys ssp. chamaedrys", "Teucrium montanum"),
          to = "Teucrium chamaedrys/montanum")
z <- list(abbr = "teuc cham mont",
          taxon = "Teucrium chamaedrys/montanum")

# display taxa again
taxon(combine(x, y, z))

}
\seealso{
\code{\link{layers}}
}
\keyword{ methods }

