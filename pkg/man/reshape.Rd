\encoding{UTF-8}

\name{reshape}

\alias{shapeSpecies}
\alias{reshapeSpecies}

\title{ Reshape Species Matrix }

\description{
  \code{reshapeSpecies} is a utility function for conversion of objects of
  class \code{"data.frame"} to class \code{"Species"}. The format demands
  cover values for layers to be given in separate columns. Such data structures
  might be delivered by certain data bases or archives, or are available as
  digitized field lists. \code{shapeSpecies} performs the reverse operation on
  objects of class \code{Vegsoup*}. This might be useful for tabular presentation
  of single \enc{relevées}{relevees}.
}

\usage{
  reshapeSpecies(x, file, schema, sep = ";", dec = ",", verbose = FALSE)

  shapeSpecies(obj)
}

\arguments{
  \item{ x }{
	A data frame given in a particular structure.
  }
  \item{ obj }{
  A \code{Vegsoup*} object.
  }
  \item{ file }{
	Path to a csv-file.
  }
  \item{ schema }{
	Names of columns to be searched for. See \sQuote{Details}.
  }
  \item{ sep }{
	See \code{\link{read.csv}} for parsing of csv files. Defaults are set
	according to \code{read.csv2}, also regarding \code{dec}!
  }
  \item{ dec }{
	See \code{read.csv}.
  }
  \item{ verbose }{
	Prints diagnostic messages.
  }
}

\details{
  The current implementation for \code{reshapeSpecies} support only a single
  case. The values of \code{schema} are seeked in the column names of
  \code{x}, respectively, \code{file} to tabulate observations for layers.
  The first element of \code{schema} identifies plots, the second species
  abbreviations or taxon names, and the remaining columns contain the
  observations on a specific layer. Layer codes are assigned based on
 \code{names(x)}. See \sQuote{Examples}.

  \code{shapeSpecies} reverts what is done by \code{reshapeSpecies}. The
  main purpose of that function is print a compact and more readable species
  list including when there are more than one layer.
}

\value{
  \code{reshapeSpecies} returns an object of class
  \code{\linkS4class{Species}}.

  \code{shapeSpecies} returns an object of class
  \code{"data.frame"}.
}

\note{
  \code{shapeSpecies} will change the order of taxa based on
  \code{taxonomy(obj)}. The order of plots equals that of the input object.
  The same is true for the order of layer columns.
}

\author{ Roland Kaiser }

\seealso{
  \code{\linkS4class{Species}},
  \code{\link{stackSpecies}}
}

\examples{
# create dummy data
# only a single plot for demonstartion
require(vegsoup)

data(barmstein)
x <- barmstein

# for demonstration, first create format
spc <- shapeSpecies(x)
class(spc)
head(spc)

# then stack again
r <- reshapeSpecies(spc, schema = c("plot", "abbr", "hl", "sl", "ml"))

}

\keyword{ import }

