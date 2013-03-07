\name{rbind}
\alias{rbind}

\title{
Combine Vegsoup* objects
}
\description{
Take a sequence of arguments of objects from class \code{Vegsoup*} and combine them into a single object.
}
\usage{
rbind(..., deparse.level = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ \dots }{
Objects of class \code{VegsoupData}.
}
  \item{ deparse.level }{
Not used.
}
}
\details{
By calling \code{rbind} all slots of the input objects are combined and a new object is returned. Note, if a list of \code{Vegsoup*} objects needs to be combined, simply call \code{do.call("rbind", list.of.Vegsoup.objects)} (see \sQuote{Examples}).
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
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# split object in three parts
s1 <- dta[1:2, ]
s2 <- dta[3:4, ]
s3 <- dta[5:6, ]

# combine them
res <- rbind(s3, s1, s2)

# equivalent
ss <- list(s1, s2, s3)
res <- do.call("rbind", ss)

# note, ordering is changed
rownames(dta) 
rownames(res)

}
\keyword{methods}

