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
  list. List elements should be named \code{from} and \code{to}. Entries must not to be named, but should otherwise be in that order.
  }
  \item{ z }{
   list. List elements should be named \code{abbr} and \code{taxon}, as in \code{taxonomy(x)}. Entries must not to be named, see above.
  }
}

\details{
This utility function can be used to perform taxonomic aggregation.
Argument \code{y} defines the taxa to be combined (list element \code{from}) and a \emph{new} taxonomic description of that aggregation (list element \code{to}).
Argument \code{z} holds the \emph{new} abbreviation (see class \code{\linkS4class{Taxonomy}}) for the aggregated taxon and a taxonomic description of that aggregation (cp. argument \code{y}).
Element \code{z} can also be missing. In this case \code{y$to} is queried from \code{taxonomy(x)}. This is the case if a lower level taxon (a subspecies) is combined with the higher level nominat form (s.lat., Genus species ssp. subspecies = Genus species s.lat.).
Note, if any \code{z$abbr} or \code{z$taxon} equals an exiting entry in \code{taxonomy(x)} it has to be quoted in \code{y$from} (see \sQuote{Examples}, for Scapania).
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

# pool to a new add-hoc taxon 
y <- list(from = c("Teucrium chamaedrys ssp. chamaedrys", "Teucrium montanum"),
          to = "Teucrium chamaedrys/montanum")
z <- list(abbr = "teuc cham mont",
          taxon = "Teucrium chamaedrys/montanum")

# display taxa again
taxon(combine(x, y, z))

data(windsfeld)
x <- windsfeld

# pooling taxa with an existing taxon
taxon(x, "Scapania")
y <- list(from = c("Scapania aequiloba", "Scapania aspera",
          "Scapania curta agg."),
          to = "Scapania curta agg.")
z <- list(abbr = "scap curt aggr",
          taxon = y$to)
taxon(combine(x, y, z), "Scapania")

# we can omit z if y$to exists in the data set
taxon(x, "Biscutella")
y <- list(from = c("Biscutella laevigata ssp. laevigata"),
          to = "Biscutella laevigata s.lat.")
taxon(combine(x, y), "Biscutella")

}
\seealso{
\code{\link{layers}}
}
\keyword{ methods }

