\encoding{UTF-8}

\name{Sites-class}
\Rdversion{1.1}
\docType{class}

\alias{sites}

\alias{Sites-class}

\alias{nrow,Sites-method}

\alias{$,Sites-method}
\alias{$<-,Sites-method}

\alias{rbind,Sites-method}

\alias{sites,Sites-method}

\alias{sites-method}
\alias{sites,character-method}
\alias{sites,data.frame-method}
\alias{sites,matrix-method}
\alias{sites,Sites-method}
\alias{sites,rbind-method}

\title{ Sites Class }

\description{
  Data representation of sites data in \emph{long format} where
  each observation on a particular plot (environmental variables,
  stand parameters) is represented by one row.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Sites", ...)},
  where \code{...} expands to slot \code{'data'}. Using the
  \code{sites} method with appropriate objects is also convenient
  (see \sQuote{Methods}). Function \code{\link{stackSites}} is usefull
  when the data is in \emph{wide} format (plot by variable matrix).
}

\section{Slots}{
  \describe{
  The \code{data} slot holds a \code{"data.frame"} with three columns,
  all of type \code{character} and with names \code{plot}, \code{variable}
  and \code{value}. The meaning of these columns is as detailed below:

  \itemize{
    \item \code{plot} contains the plot identifier associated with each
      observation (cf. \code{\linkS4class{Species}}).
    \item \code{variable} contains a name for the measured variable.
    \item \code{value} holds the value of a particular variable.
  }  
  \item{\code{data}:}{Object of class \code{"data.frame"}
  with three columns named
  \code{'plot'}, \code{'variable'} and \code{'value'}. }
  }
}

\section{Methods}{
  \describe{
    \item{ sites }{
      There exists \code{species} methods for a number of R base classes:

      \code{signature(obj = "data.frame")}:
        promote \code{data.frame} to object of class \code{"Sites"}

      \code{signature(obj = "matrix")}:
        promote \code{matrix} to object of class \code{"Sites"}
        Note, \code{obj} does not need to have \code{dimnames}.

      \code{signature(obj = "character")}:
        supply a file to be read by \code{read.csv} and promote that object to
        class \code{"Sites"}. Remember to pass appropriate arguments to
        \code{\link{read.csv}} that meet the specifications of your file (e.g. argument \code{sep}).

      Only the Sites class has a \code{sites} method so far.

      \code{signature(obj = "Sites")}:
        accessor function (\emph{slot "data"}).
    }

    \item{ nrow }{
      \code{signature(x = "Sites")}:
        the number of rows. Each plot and variable combination constitutes a row.
    }

    \item{ rbind }{
      \code{signature(x = "Sites")}:
        \code{rbind} method to combine two or more objects.
    }

  }
}

\author{ Roland Kaiser }

\seealso{
  \code{\link{stackSites}}
  \code{\linkS4class{Species}}
  \code{\linkS4class{Vegsoup}}
}

\examples{
require(vegsoup)
showClass("Sites")

data(barmstein)
x <- barmstein

# extract from Vegsoup object for demonstration
# get sites slot, it is returned in wide format
s <- Sites(barmstein)

# add plot names
s <- cbind(plot = rownames(x), s)
class(s)

#	promote to class Sites
s <- stackSites(s)

# get slot 'data' from object of class 'Sites'
s.df <- sites(s)
class(s.df)

# matrix method 
s.m <- as.matrix(s.df)
s <- sites(s.m)

# combine objects
data(windsfeld)

s2 <- stackSites(cbind(plot = rownames(windsfeld), Sites(windsfeld)))

rbind(s, s2)


}

\keyword{ classes }

