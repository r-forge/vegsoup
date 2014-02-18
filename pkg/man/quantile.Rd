\name{quantile}
\docType{methods}

\alias{quantile}
\alias{quantile,VegsoupPartition-method}

\title{ Sample Quantiles for a Community Matrix }

\description{
Returns sample quantiles corresponding to the given probabilities for each species in each partition.
}

\usage{
\S4method{quantile}{VegsoupPartition}(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7,
          coverscale = FALSE, ...)
}

\arguments{
  \item{ x }{
    A \code{VegsoupPartition} object.
  }
  \item{ probs }{
    numeric vector of probabilities. See \code{\link[stats:quantile]{quantile}}.
  }
  \item{ na.rm }{
    Not used. A community matrix in \pkg{vegsoup} is not allowd to have \code{NAs}!
  }
  \item{ names }{
    Not used.
  }  
  \item{ type }{
    Selecting one of the nine quantile algorithms. See \code{\link[stats:quantile]{quantile}}.
  }
  \item{ coverscale }{
    Recode abundances and return orginal scale. Only applies if \code{is.ordinal(x)}
    evaluates to \code{TRUE}.
  }
  \item{ \dots }{
    Not used.
  }
}

\details{
%%There is special meaning of argument \code{na.rm} that is used to handle missing observations of species in the community matrix. Those absences are conveniently coded as zeros. If \code{na.rm = TRUE} all zeros are replaced by \code{NA} and the function returns \code{NA} for absence of a particular species in a respective cluster. If \code{na.rm = FALSE} zeros are treated as is.
If the input object has layers the summary statistics are calculated for each layer. If abundances are ordinal \code{as.numeric(obj)} is used to obtain numeric values for calculation. If \code{coverscale = TRUE} results are back converted to ordinal scale (e.g. Braun-Blanquet scale), otherwise will be returned as class midpoints as defiuned in \code{coverscale(x)}. Note, suppling presence/absence data is supported, however, results may be meaningless.
}
\value{
An array with as many dimensions as \code{length(probs)} containing the summary information of  \code{\link[stats:quantile]{quantile}} for a species by cluster matrix.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{boxplot.stats}}, \code{\link{fivenum}}, \code{\linkS4class{Coverscale}}, \code{\link{Latex}}.
}

\examples{

require(vegsoup)

data(barmstein)
x <- VegsoupPartition(barmstein, k = 2)

x1 <- quantile(x, probs = seq(0, 1, 0.25))
x1[ , , 3]

x2 <- quantile(x, probs = c(0, 0.5, 1), coverscale = TRUE)

x2[ , , 2] # probs[2]

}

\keyword{ univar }

