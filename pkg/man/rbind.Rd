\name{rbind}
\alias{rbind}

\title{
Combine Objects of Class \sQuote{VegsoupData}
}
\description{
Take a sequence of arguments of objects from class \code{VegsoupData} and combine them.
}
\usage{
rbind(..., deparse.level = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Objects of class \code{VegsoupData}.
}
  \item{deparse.level}{
Not used.
}
}
\details{
By calling \code{rbind} all slots of the input objects are combined and a new obejct is returned.
}
\value{
On object of class \code{VegsoupData}.
}
\section{Warning }{
The method raises an error if plot names are not unique after combining objects.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{VegsoupData-class}}
}
\examples{
data(testdata)
dta <- VegsoupData(Vegsoup(species, sites, taxonomy,
	scale = list(scale = "Braun-Blanquet")))

# split object in three parts		
s1 <- dta[1:2, ]
s2 <- dta[3:4, ]
s3 <- dta[5:6, ]

# combine them
res <- rbind(s3, s1, s2)

# note, ordering is changed
rownames(dta) 
rownames(res)

}
\keyword{methods}

