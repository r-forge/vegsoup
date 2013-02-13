\name{merge}
\alias{merge}

\title{
Combine Objects of Class \sQuote{Vegsoup}
}
\description{
Take a sequence of arguments of objects from class \code{Vegsoup} and combine them.
}
\usage{
merge(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    Merge \code{Vegsoup*} objects.
}
}
\details{
  By calling \code{merge} all input objects are combined and a new object is
  returned.
}
\value{
  An object of class \code{Vegsoup}.
}
\section{ Warning }{
  The method raises an error if plot names are not unique after combining
  objects.
}
\author{
  Roland Kaiser
}
\seealso{
  \code{link{merge}},\code{\linkS4class{VegsoupData}}
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
res <- merge(s3, s1, s2)

# note, ordering is changed
rownames(dta) 
rownames(res)

}
\keyword{methods}

