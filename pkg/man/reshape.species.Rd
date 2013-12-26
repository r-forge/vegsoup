\name{reshape}

\alias{shape.species}
\alias{reshape.species}

\title{ Reshape Stacked Species Matrix With Layers in Separate Columns }

\description{
  \code{reshape.species} is a utility function for conversion of objects of
  class \code{"data.frame"} to class \code{"Species"}. The format demands
  cover values for layers to be given in separate columns.
  \code{shape.species} performs the reverse operation on objects of class
  \code{Vegsoup*}.
}

\usage{
  reshape.species(x, file, schema, sep = ";", dec = ",", verbose = FALSE)
  shape.species(obj)
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
  The current implementation for \code{reshape.species} support only a single
  case. The values of \code{schema} are seeked in the column names of
  \code{x}, respectively, \code{file} to tabulate observations for layers.
  The first element of \code{schema} identifies plots, the second species
  abbreviations or taxon names, and the remaining columns contain the
  observations on a specific layer. Layer codes are assigned based on
 \code{names(x)}. See \sQuote{Examples}.

  \code{shape.species} reverts what is done by \code{reshape.species}. The
  main purpose of that function is print a compact and more readable species
  list including when there are more than one layer.
}

\value{
  \code{reshape.species} returns an object of class
  \code{\linkS4class{Species}}.

  \code{shape.species} returns an object of class
  \code{"data.frame"}.
}

\note{
  \code{shape.species} will change the order of taxa based on
  \code{Taxonomy(obj)}. The order of plots equals that of the input object.
  The same is true for the order of layer columns.
}

\author{ Roland Kaiser }

\seealso{
  \code{\linkS4class{Species}},
  \code{\link{stackSpecies}}
}

\examples{
data(testdata)

# create dummy data
# only a single plot for demonstartion
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# for demonstration, first create format
spc <- shape.species(dta)
# then stack again
res <- reshape.species(spc, schema = c("plot", "abbr", "hl", "sl", "ml"))

}

\keyword{ import }
