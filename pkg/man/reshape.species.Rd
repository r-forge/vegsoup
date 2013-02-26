\name{reshape}
\alias{reshape.species}

\title{
Reshape Already Stacked Species Matrix Where Abundance Values for Layers are in Separate Columns
}
\description{
Utility function for conversion to class \code{"Species"}.
}
\usage{
reshape.species(x, file, layers, csv2 = TRUE, verbose = TRUE)
}

\arguments{
  \item{ x }{
    A data frame given in a particular structrue.
  }
  \item{ file }{
    Path to a csv-file.
  }
  \item{ schema }{
    Names of columns to be searched for. See \sQuote{Details}.
  }
  \item{ csv2}{
    Use \code{read.csv2} instead of \code{read.csv}, defaults to \code{TRUE}
  }
  \item{ verbose}{
    Prints diagnostic meassages.
  }   
}
\details{
  The current implementation support only one case. The values of
  \code{schema} are seeked in the column names of \code{x}, respectively,
  \code{file} to tabulate observations for layers. The first element of
  \code{schema} idientifies plots, the second species abbreviations or taxon
  names, and the remaining columns contain the observations per layer. See
  \sQuote{Examples}.
}
\value{
  Returns an object of class \code{\linkS4class{Species}}.
}

\author{ Roland Kaiser }

\seealso{
  \code{\linkS4class{Species}},
  \code{\link{stack.species}}
}

\examples{
data(testdata)

# create dummy data
# only a single plot for demonstartion
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")[2, ]

# explode what is already in shape
res <- as.data.frame(
	matrix("",
	ncol = length(Layers(dta)) + 2, # we need 2 columns for plot and abbr
	nrow = richness(dta)),
	stringsAsFactors = FALSE)
names(res) <- c("plot", "abbr", Layers(dta))
res$plot <- rep(rownames(dta), richness(dta, "sample"))	
res$abbr <- unlist(sapply(1:nrow(dta), function (x) Taxonomy(dta[x, ])$abbr))

spc <- Species(dta)

for(i in 1:nrow(spc)) {
	#	i = 90
	tmp <- spc[i, ]
	sel <- res$plot == tmp$plot & res$abbr == tmp$abbr
	res[sel, match(tmp[3], names(res))] <- tmp[4]
}

res <- reshape.species(res, schema = c("plot", "abbr", "hl", "sl", "ml"))

}
\keyword{ import }

