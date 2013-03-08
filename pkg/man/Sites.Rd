\name{Sites}
\docType{methods}
\alias{Sites}
\alias{Sites-methods}
\alias{Sites<-}  
\alias{Sites<--methods}


\title{
Methods to Retrieve or Manipulate the Sites Slot of an Object of Class Vegsoup*
}
\description{
Accessor and replacement methods acting on the \code{'sites'} slot.
}
\usage{
Sites(obj)
Sites(obj) <- value
}

\arguments{
  \item{ obj }{
    object of class \code{Vegsoup*}
  }
  \item{ value }{
    object of class \code{"data.frame"}.
  }

}
\details{
The value object in the replacement function needs to have rownames matching \code{rownames(obj)}. The order is respected and leads to reordering of the object to which the value object is assigned.
}
\value{
  An object of class depending on the input class.
}
\author{ Roland Kaiser }
\seealso{
\code{\linkS4class{Vegsoup}}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# use replacemnet to shuffe order
(x <- rownames(dta))
Sites(dta) <- Sites(dta)[sample(rownames(dta)), ]
(y <- rownames(dta))
x;y
}
\keyword{ methods }

