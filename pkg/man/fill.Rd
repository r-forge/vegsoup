\encoding{UTF-8}

\name{fill}
\docType{methods}

\alias{fill}
\alias{fill,Vegsoup-method}

\title{ Matrix fill of species matrix }

\description{
  Calculates the proportion (percentage) of all non zero elements in a
  species matrix.
}
\usage{
  fill(obj)
}
\arguments{
  \item{ obj }{
	\code{Vegsoup*} object.
  }
}
\value{
  returns a vector of length one and mode \code{"numeric"} 
}
\author{ Roland Kaiser }
\seealso{
  \code{\link{nestedtemp}} in \pkg{vegan} for nestedness indices.
}
\examples{
require(vegsoup)

data(barmstein)

fill(barmstein)

# relation of matrix fill and number of occurences
data(windsfeld)

# unify layer and transform to presence/absence
x <- compress(windsfeld)

# number of occurences
jj <- colSums(x)
i <- sort(unique(jj))

# compute matrix fill for each occurence treshold
r <- t(sapply(i, function (y) {
	xi <- x[ , match(names(jj[ jj >= y ]), colnames(x)) ]
	c(dim(xi), fill(xi))
}))

par(mfrow = c(2,2))
plot(i, r[, 1], type = "b",
	xlab = "occurence threshold", ylab = "number of sites")

plot(i, r[, 2], type = "b",
	xlab = "occurence threshold", ylab = "number of species")

plot(i, r[, 1] / r[, 2], type = "b",
	xlab = "occurence threshold", ylab = "number of sites / number of species")
	
plot(i, r[, 3], type = "b",
	xlab = "occurence threshold", ylab = "matrix fill")
dev.off()
}
\keyword{ methods }

