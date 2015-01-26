\encoding{UTF-8}

\name{as.matrix}

\alias{as.logical}
\alias{as.logical,Vegsoup-method}

\alias{as.numeric}
\alias{as.numeric,Vegsoup-method}

\alias{as.character}
\alias{as.character,Vegsoup-method}

\alias{as.matrix}
\alias{as.matrix,Vegsoup-method}

\alias{as.array}
\alias{as.array,Vegsoup-method}

\alias{as.vector}
\alias{as.vector,Vegsoup-method}
\alias{as.vector,Vegsoup,missing-method}

\alias{indices}
\alias{indices,Vegsoup-method}

\title{ Retrieve Species Matrix with Specified Storage Mode }

\description{
The methods retrieve a community table as class \code{'matrix'} and of mode according to the methods literal name.  
Method \code{as.matrix} returns corresponding types of matrices but specified by an argument \code{typeof}, the same holds true for \code{as.array}.  
The \code{indices} method extracts the (possibly converted) values and row and column indices of these in a matrix (sparse matrix representation), as well as corresponding row and column names.  
The latter have a special meaning in all methods, since they carry an attribute assigning species to vegetation layer or strata (cross-classifying factors).  
}

\usage{
\S4method{as.logical}{Vegsoup}(x, mode)

\S4method{as.numeric}{Vegsoup}(x, mode)

\S4method{as.character}{Vegsoup}(x, mode)

\S4method{as.matrix}{Vegsoup}(x, typeof, ...)

\S4method{as.array}{Vegsoup}(x, typeof, ...)

\S4method{indices}{Vegsoup}(x, typeof)

\S4method{as.vector}{Vegsoup}(x, mode)
}

\arguments{
  \item{ x }{
    Vegsoup object.
  }
  \item{ mode }{
    character. If \code{mode = "R"} the method returns the transposed matrix. If missing, the default
    no transposition happens (\code{mode = "Q"}). Lower and upper case letters are treated the same.
    See \sQuote{Details}.
  }
  \item{ typeof }{
    Selects the desired storage mode or type of the object.
    Can be one of \code{"character"}, \code{"numeric"} or \code{"logical"}.
  }
  \item{ \dots }{
    Arguments passed to \code{as.logical} etc. to supply a value for
    argument \code{mode}. See argument \code{mode}.
  }
}

\details{
For any object inheriting from class \code{Vegsoup} the methods return an object of class \code{'matrix'} with \code{dimnames} giving plot and species names.  
The latter are strings where any occurrence in a layer is labeled using the layer name and the species abbreviation, collapsed with the \code{"@"} glyph (layer-replicate or pseudo-species, see \code{\link{layers}}, \code{\link{splitAbbr}} and \code{\link{abbr}}).  

The object returned by a method (except \code{indices} and \code{as.vector}) is suitable as input to functions in other packages that demand an object of class \code{'matrix'}.  
Dispatch is guaranteed if these functions internally coerce objects to this class by calls to \code{as.matrix}.  
Besides that, it is possible to supply a Vegsoup* object to a base function by wrapping it into a respective method (see \sQuote{Examples}).  
As there is no way to specify the \code{typeof} or \code{mode} arguments in a call to a base function the S3 method for \code{as.matrix()} uses \code{typeof = "numeric"} as default, which is suitable for most applications.  
If the user wants to ensure that presence/absence data should be returned by calls to \code{as.matrix} inside functions of other packages , simply assign a standardization method beforehand (e.g. \code{decostand(x) <- "pa"}).  
See also \code{\link{as.data.frame}} for passing data to model-fitting and plotting functions.  

Note, the behavior of the \code{as.numeric} and two related \code{as.*} methods is rather untypical, in the way that they do not return vectors but also matrices (see below)!  
The idea here is to simplify the writing of code.  
Certainly, the \code{as.vector} method returns a vector of length \code{ncell(x)}, where the storage mode can be controlled by argument \code{mode}.  
Let's recall the above mentioned \code{as.*} methods as \code{as.matrix} calls these internally:  

\code{as.numeric} returns a matrix of mode \code{"numeric"} with species abundances coded by \code{coverscale(obj)@lims}.  
Absences are zeros.  
Equals \code{as.matrix(x, typeof = "numeric")}.  

\code{as.character} returns a matrix of mode \code{"character"} with species abundances in the data set's original scale (\code{coverscale(x)@codes}).  
Absences are coded as zeros not dots!  
Equals \code{as.matrix(x, typeof = "character")}.  

\code{as.logical} returns a matrix of mode \code{"numeric"} with zeros coding for species absences and ones for presences.  
Equals \code{as.matrix(x, mode = "logical")}.  

It has to be noted that, \code{as.logical} and \code{as.matrix(x, "logical")} do not return a boolean or logical matrix composed of \code{TRUE} and \code{FALSE} but use ones to code presences (\code{TRUE}) and zeros (\code{FALSE}) for absences.  
The \code{mode} (\code{storage.mode}) of the returned matrix is of type \code{"numeric"} (more specifically of mode \code{"integer"}).  

As outlined above, the somehow misleading named argument \code{mode} refers to the type of analysis intended on the returned object (recall, \code{"typeof"} determines \code{mode} (\code{storage.mode}) of the returned matrix).  
This is due to constraints implied by the definition of the imported generic methods.  
The argument \code{mode} can be passed to all methods and has two possible values.  
If \code{mode = "Q"} pairs of objects are compared and the analysis is said to be in \emph{R mode} --
this is what users usually want or expect.  
When pairs of descriptors are compared, the analysis is said to be in \emph{Q mode} (Legendre & Legendre 2012).  
For example, \code{as.matrix(x, "logical", "R")} returns a transposed presence/absence matrix.  
Of course, this can also be written as \code{t(as.matrix(x, "logical"))} or \code{as.logical(x, mode = "R")}.  

The \code{as.array} method is useful when an object having more than one layer should be represented as a multidimensional dimensional array, where there are as many dimensions as there are layers -- method \code{as.array} returns such an array.  
Note, the order in which species appear in the column names is alphabetic and there is currently no implementation of the \code{'mode'} argument to simultaneously transpose the array!  

Finally, the \code{indices} method returns the locations (row and column pointers) and values of nonzero entries in a matrix.  
Those indices can be used to construct a \code{\link{sparseMatrix}}. Coercion method \code{as(x, "sparseMatrix")} helps in doing so.  

Summary of the meaning of arguments \code{mode} and \code{typeof} for all methods.

\tabular{lll}{
\emph{method} \tab \emph{mode} \tab \emph{typeof} \cr
 \tab \tab \cr
as.logical, as.numeric, as.character \tab Q or R mode analysis \tab defined by literal name \cr
as.matrix, as.array \tab Q or R mode analysis \tab character, numeric or logical \cr
as.vector \tab character, numeric or logical \tab not defined \cr
indices \tab not defined \tab character, numeric or logical \cr
}

}
\value{
A \code{'matrix'} (or \code{'array'}) of mode \code{"numeric"} or \code{"character"}. See \sQuote{Details}.  

For \code{as.vector}, a vector of length of \code{ncell(x)}.  
}
\note{
If the object has non \code{NULL} values in its data standardization slot the method defined by \code{decostand(x)} applies!
}

\author{ Roland Kaiser }

\references{
Legendre, P. and Legendre, L. (2012). Chapter 7 in Numerical ecology. Developments in environmental modeling, 24.
}
\seealso{
\code{\link{as.data.frame}} for passing data to model-fitting and plotting functions,
\code{\link{as.data.frame}},
\code{\link{Vegsoup}},
\code{\link{decostand}} and
\code{\link{Coverscale}}
}
\examples{
require(vegsoup)
data(barmstein)

x <- barmstein

# presence/absence representation
x.pa <- as.logical(x)
# the same results can be achieved using the classes decostand property
decostand(x) <- "pa"

# note, results of as.numeric() and as.logical()
# are equal if presence/absence (transformation) is applied
x.pa2 <- as.numeric(x)

# for a true boolean matrix change mode
mode(x.pa) <- "logical"
head(x.pa)
all.equal(x.pa, x.pa2, check.attributes = FALSE)

# using as.matrix and argument mode
as.matrix(x, typeof = "character", mode = "R")

# compact display
as(as.matrix(x, "logical", "R"), "sparseMatrix")

pro <- protest(as.logical(x), as.numeric(x))

# objects of class Vegsoup used in arguments of base functions
# where an object of class matrix is expected
decostand(x) <- "hellinger"
decorana(x)
decostand(x) <- NULL # reset the default

# species matrix representations and memory demands
# class "matrix"
s1 <- object.size(as.matrix(x))             # most efficient
# class "list" (pointers and values of all non zero entries)
s2 <- object.size(indices(x))               # almost equal
# coercion to class "sparseMatrix" is based on method indices
s3 <- object.size(as(x, "sparseMatrix"))    # efficient
# the long format as stored in Vegsoup* objects (slot 'species')
s4 <- object.size(species(x))               # most general but least efficent
# a multi-dimensional array of layer replicates
s5 <- object.size(as.array(x))              # least efficent

( s <- data.frame(size = as.numeric(c(s1, s2, s3, s4, s5)),
                  row.names = c("matrix", "indices", "sparseMatrix", "Species", "array")) )
barplot(t(s), ylab = "object size")

# comparison of memory demands of objects of other classes
s1 <- object.size(as(x, "data.list"))       # least efficent, but highly generic
s2 <- object.size(as(x, "mefa"))            # more efficent, but less generic
s3 <- object.size(as.data.frame(x))         # efficent, but rather meaningless
# for comparison subtract slots with sp objects
sp.object.size <- object.size(x@sp.points) + object.size(x@sp.polygons)
s4 <- object.size(x) - sp.object.size       # most general and most efficent

( s <- data.frame(size = as.numeric(c(s1, s2, s3, s4)),
                  row.names = c("data.list", "mefa", "data.frame", "vegsoup")) )
barplot(t(s), ylab = "object size")
}
\keyword{ methods }
\keyword{ manip }

