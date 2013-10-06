\name{row+colnames}
\alias{rownames}
\alias{rownames-methods}
\alias{rownames<-,Vegsoup,character-method}
\alias{rownames<-,Vegsoup,integer-method}
\alias{row.names}
\alias{row.names-methods}
\alias{row.names<-,Vegsoup,integer-method}
\alias{colnames}
\alias{colnames-methods}
\alias{dimnames}
\alias{dimnames-methods}
\alias{names}
\alias{names-methods}
\alias{names<-,Vegsoup,character-method}
\alias{$}
\alias{$-methods}
\alias{$<-}

\title{ Row and Column Names of \code{Vegsoup*} objects }
\description{
Retrieve the row or column names of the species \code{matrix} and the names of the sites \code{data.frame}. Note, the species matrix and the sites table relate to each other by a 1:1 data base schema. Therefore \code{rownames(obj)} (the plots) apply to both the species matrix and the sites data frame.
}
\usage{
## slot 'species'
\S4method{dimnames}{Vegsoup}(x)
\S4method{rownames}{Vegsoup}(x, do.NULL = TRUE, prefix = "row")
\S4method{colnames}{Vegsoup}(x, do.NULL = TRUE, prefix = "col")
## S4 replacement method for signature 'Vegsoup*'
rownames(x) <- value

## slot 'sites'
\S4method{row.names}{Vegsoup}(x)
\S4method{names}{Vegsoup}(x)
x$name
## S4 replacement method for signature 'Vegsoup*'
row.names(x) <- value
names(x) <- value
x$i <– value
}
\arguments{
  \item{ x }{
    a \code{Vegsoup*} object. 
  }
  \item{ name }{
    A literal character string or a \link[base:name]{name}.
  }
  \item{ do.NULL }{
    Not used.
  }
  \item{ prefix }{
    Not used.
  }
  \item{ value }{
    A character or integer vector of the same length as \code{rownames(x)}, or NULL.
  }
}
\details{
Function \code{dimnames} returns \code{dimnames(as.matrix(x))}. Correspondingly, \code{colnames} equals \code{dimnames(as.matrix(x))[[2]]}. This applies only to the species matrix which is of class \code{matrix}. Note, function \code{names} if called on an object of class \code{matrix} will return \code{NULL} in base R. Therefore the \code{names} method is only defined for the sites slot and equals \code{names(Sites(x))}. As both, the species matrix and the sites data frame, have the same number of rows (plots) in exactly the same order, the \code{\link[base:rownames]{rownames}} and \code{\link[base:row.names]{row.names}} methods  return identical values for both data partitions (matrices and data.frames respectively).

The \code{"$"} operator can be used to access columns of the sites slot which is of class \code{data.frame} and has also functions to get or set the names of the object. Indexing of the species matrix can be done using indices specifying elements to extract. See \sQuote{Examples}.

}
\note{
Assigment of new rownames can be dangerous and should only be performed with
care! Of course, valid rownames have no duplicates nor missing values.
}
\author{ Roland Kaiser }
\seealso{
\code{\link{DecomposeName}
}
}
\examples{
library(vegsoup)
data(testdata)
dta <- Vegsoup(spc, sts, txa, "braun.blanquet")

# access dimnames of species matrix (class 'matrix')
colnames(dta) # species-layer replicates
rownames(dta) # plot names
dimnames(dta)

# return names of sites data (class 'data.frame')
names(dta)
row.names(dta)

# access names of sites data
dta$rock

# set a name
names(dta)[1] <- "bar"

# assign a value
if (any(names(dta) == "rock")) {
   dta$rock <- sqrt(dta$rock)
}

# return plot names
rownames(dta)

# assign new rownames
rownames(dta) <- paste("new", rownames(dta), sep = ".")
}
\keyword{ methods }
