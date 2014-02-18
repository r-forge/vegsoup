\name{row+colnames}

\alias{rownames}
\alias{rownames,Vegsoup-method}
\alias{rownames,Vegsoup,missing,missing-method}

\alias{rownames<-}
\alias{rownames<-,Vegsoup-method}
\alias{rownames<-,Vegsoup,integer-method}
\alias{rownames<-,Vegsoup,character-method}
\alias{rownames<-,Vegsoup,missing,missing-method}

\alias{row.names}
\alias{row.names,Vegsoup-method}

\alias{row.names<-}
\alias{row.names<-,Vegsoup-method}
\alias{row.names<-,Vegsoup,integer-method}
\alias{row.names<-,Vegsoup,character-method}

\alias{colnames}
\alias{colnames,Vegsoup-method}

\alias{dimnames}
\alias{dimnames,Vegsoup-method}

\alias{names}
\alias{names,Vegsoup-method}

\alias{names<-}
\alias{names<-,Vegsoup-method}
\alias{names<-,Vegsoup,character-method}

\alias{$}
\alias{$,Vegsoup-method}
\alias{$,character-method}
\alias{$,Vegsoup,character-method}

\alias{$<-}
\alias{$<-,Vegsoup-method}
\alias{$<-,character,ANY-method}
\alias{$<-,Vegsoup,character-method}

\title{ Row and Column Names of \code{Vegsoup*} objects }

\description{
Retrieve the row or column names of the species matrix (see \code{\link[vegsoup]{as.matrix}}) or the names of the sites data (see \code{\link{Sites}}). Note, the \emph{species} matrix and the \emph{sites} data frame relate to each other by a 1:1 data base schema. Therefore, \code{rownames(x)} (matrix method) and \code{row.names(x)} (data.frame method) return the same and the second method just exists for completeness.
}

\usage{
## slot 'species'

\S4method{dimnames}{Vegsoup}(x)

\S4method{rownames}{Vegsoup}(x, do.NULL = TRUE, prefix = "row")

\S4method{colnames}{Vegsoup}(x, do.NULL = TRUE, prefix = "col")

\S4method{rownames}{Vegsoup}(x) <- value

\S4method{row.names}{Vegsoup}(x)

\S4method{names}{Vegsoup}(x)

\S4method{$}{Vegsoup}(x, name)

## slot 'species'
\S4method{row.names}{Vegsoup}(x) <- value

\S4method{names}{Vegsoup}(x) <- value

\S4method{row.names}{Vegsoup}(x) <- value

\S4method{$}{Vegsoup}(x, name) <- value
}

\arguments{
  \item{ x }{
    \code{Vegsoup*} object. 
  }
  \item{ name }{
    character. Literal character string or a \link[base]{name}.
  }
  \item{ do.NULL }{
    not used.
  }
  \item{ prefix }{
    not used.
  }
  \item{ value }{
    character or integer vector of the same length as \code{rownames(x)}, or \code{NULL}.
  }
}

\details{
Function \code{dimnames} returns \code{dimnames(as.matrix(x))}. Correspondingly, \code{colnames} equals \code{dimnames(as.matrix(x))[[2]]}. This applies only to the species matrix which is of class \code{matrix}. Note, function \code{names} if called on an object of class \code{matrix} will return \code{NULL} in base R. Therefore the \code{names} method is only defined for the sites slot and equals \code{names(Sites(x))}. As both, the species matrix and the sites data frame, have the same number of rows (plots) in exactly the same order, the \code{\link[base]{rownames}} and \code{\link[base]{row.names}} methods  return identical values for both data partitions (matrices and data.frames respectively).

The \code{"$"} operator can be used to access columns of the sites slot which is of class \code{data.frame} by name and has also functions to get or set the names of the object. Indexing of the species matrix can be done using indices specifying elements to extract. See \sQuote{Examples}.

}

\note{
  Assigment of new rownames can be dangerous and should only be performed with
  care! Of course, valid rownames must have no duplicates nor missing values.
}

\author{ Roland Kaiser }

\seealso{
\code{\link{taxon}
}
}

\examples{
require(vegsoup)

data(barmstein)
x <- barmstein

# access dimnames of species matrix (class 'matrix')
colnames(x) # species-layer replicates
rownames(x) # plot names
dimnames(x)

# return names of sites data (class 'data.frame')
names(x)
row.names(x)

# access names of sites data
x$rock

# set a name
names(x)[1] <- "bar"

# assign a value
if (any(names(x) == "rock")) {
   x$rock <- sqrt(x$rock)
}

# return plot names
rownames(x)

# assign new rownames
rownames(x) <- paste("new", rownames(x), sep = ".")
}
\keyword{ methods }

